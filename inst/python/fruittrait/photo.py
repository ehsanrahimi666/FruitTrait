"""Photo -> fruit outlines -> fruit traits (back end of the FruitTrait R package).

Stage 1  Grounding DINO-base ("fruit. berry.") on the full photo -> confident fruit boxes. The fruit display is the
         box around the best box and the confident boxes whose centres lie within display_reach box sizes of it.
Crop     square crop around the display (+15% margin) at native resolution (long side <= 1024 px).
Stage 2  Grounding DINO-base again inside the crop -> fruit boxes. Boxes that enclose two or more smaller boxes
         (score >= cluster_evidence) are whole clusters and are set aside; the other boxes scoring >= conf_threshold
         and >= rel_score x the best of them are reduced by non-maximum suppression, and up to max_fruits boxes are
         kept by score.
Outline  SAM 2.1-B prompted with every kept box and with two jittered copies of the best box (one image embedding);
         each mask is clipped to its box (+5%), reduced to its largest component and filled; single fruits pass when
         they are large enough in the photo (min_fruit_px, about 32 px across), fill enough of their box, are compact,
         do not touch the crop border and are separate (at most max_overlap of their pixels shared with other
         fruits). With three or more passing fruits, a fruit more than max_size_ratio times larger than the median
         of the others is rejected (an undetected cluster). The fruit mask is the union of the fruits that pass. With
         one stage, the stage-1 boxes inside the crop are used.

All functions take plain numpy RGB images, so they can be called from R through reticulate.
"""
import numpy as np
import cv2
import torch
from PIL import Image as PILImage

from .colour import ciede2000, colour_metrics, colour_name, estimate_illuminant

DEFAULTS = dict(
    detector="IDEA-Research/grounding-dino-base", prompts=["fruit", "berry"], det_threshold=0.05,
    conf_threshold=0.20, rel_score=0.4, cluster_evidence=0.15, display_reach=2.0, cluster_n=2, nms_iou=0.3,
    max_fruits=12, max_size_ratio=6.0,
    sam_weights="sam2.1_b.pt", jitter=[0.9, 1.1],
    photo_max_side=2048, crop_margin=0.15, crop_max_side=1024,
    stage1_size={"shortest_edge": 480, "longest_edge": 800}, stage2_size=None,
    two_stage=True, min_fruit_px=800, min_fill=0.35, min_solidity=0.80, max_border=0.10, max_overlap=0.20,
    min_union_px=800,
    edge_tol=0.01, fp16=False,
)


class Models:
    """Detector and segmenter, loaded once."""

    def __init__(self, cfg=None, device=None):
        from transformers import AutoModelForZeroShotObjectDetection, AutoProcessor
        from ultralytics import SAM
        self.cfg = {**DEFAULTS, **(cfg or {})}
        self.device = device or ("cuda" if torch.cuda.is_available() else "cpu")
        self.proc = AutoProcessor.from_pretrained(self.cfg["detector"])
        self.det = AutoModelForZeroShotObjectDetection.from_pretrained(self.cfg["detector"]).to(self.device).eval()
        self.sam = SAM(self.cfg["sam_weights"])


def load_rgb(path, max_side=None):
    """Read a photo as RGB uint8 (non-ASCII Windows paths work); optional area-averaged reduction."""
    buf = np.fromfile(str(path), np.uint8)
    img = cv2.imdecode(buf, cv2.IMREAD_COLOR) if buf.size else None
    if img is None:
        return None
    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    if max_side:
        s = max_side / max(img.shape[:2])
        if s < 1:
            img = cv2.resize(img, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
    return img


def box_area(b):
    return max(0.0, b[2] - b[0]) * max(0.0, b[3] - b[1])


def box_iou(a, b):
    x1, y1, x2, y2 = max(a[0], b[0]), max(a[1], b[1]), min(a[2], b[2]), min(a[3], b[3])
    inter = max(0.0, x2 - x1) * max(0.0, y2 - y1)
    u = box_area(a) + box_area(b) - inter
    return inter / u if u > 0 else 0.0


def inside_frac(inner, outer):
    """Share of box `inner` that lies inside box `outer`."""
    x1, y1, x2, y2 = max(inner[0], outer[0]), max(inner[1], outer[1]), min(inner[2], outer[2]), min(inner[3], outer[3])
    a = box_area(inner)
    return max(0.0, x2 - x1) * max(0.0, y2 - y1) / a if a > 0 else 0.0


def scale_box(b, f, w, h):
    cx, cy, bw, bh = (b[0] + b[2]) / 2, (b[1] + b[3]) / 2, (b[2] - b[0]) * f, (b[3] - b[1]) * f
    return [max(0.0, cx - bw / 2), max(0.0, cy - bh / 2), min(float(w), cx + bw / 2), min(float(h), cy + bh / 2)]


def mask_iou(a, b):
    u = (a | b).sum()
    return float((a & b).sum() / u) if u else 0.0


def detect(models, imgs, size=None):
    """Grounding DINO on a list of RGB images -> list of (boxes Nx4 px, scores N)."""
    cfg, dev = models.cfg, models.device
    th, text = cfg["det_threshold"], ". ".join(p.lower() for p in cfg["prompts"]) + "."
    kw = {"size": size} if size else {}
    inp = models.proc(images=[PILImage.fromarray(i) for i in imgs], text=[text] * len(imgs), return_tensors="pt",
                      **kw).to(dev)
    with torch.no_grad(), torch.autocast(dev, dtype=torch.float16, enabled=bool(cfg["fp16"]) and dev == "cuda"):
        out = models.det(**inp)
    sizes = [i.shape[:2] for i in imgs]
    try:
        res = models.proc.post_process_grounded_object_detection(out, inp["input_ids"], threshold=th, text_threshold=th,
                                                                 target_sizes=sizes)
    except TypeError:
        res = models.proc.post_process_grounded_object_detection(out, inp["input_ids"], box_threshold=th,
                                                                 text_threshold=th, target_sizes=sizes)
    outs = []
    for r, (h, w) in zip(res, sizes):
        b = r["boxes"].detach().float().cpu().numpy().reshape(-1, 4)
        s = r["scores"].detach().float().cpu().numpy().reshape(-1)
        b[:, [0, 2]] = b[:, [0, 2]].clip(0, w)
        b[:, [1, 3]] = b[:, [1, 3]].clip(0, h)
        outs.append((b, s))
    return outs


def confident(b, s, cfg):
    """Indices of confident boxes: score >= conf_threshold and >= rel_score x best score (sorted by score)."""
    if len(s) == 0:
        return np.array([], int)
    keep = np.where((s >= cfg["conf_threshold"]) & (s >= cfg["rel_score"] * s.max()))[0]
    return keep[np.argsort(-s[keep])]


def single_fruit_boxes(b, s, cfg):
    """Boxes of single fruits. Boxes scoring >= conf_threshold that enclose >= cluster_n smaller boxes scoring >=
    cluster_evidence are whole clusters and are set aside; the others are kept when they score >= rel_score x the
    best of them, after non-maximum suppression, at most max_fruits, in decreasing score. Returns (indices, number of
    cluster boxes)."""
    strong = np.where(s >= cfg["conf_threshold"])[0]
    weak = np.where(s >= cfg["cluster_evidence"])[0]
    singles, n_cluster = [], 0
    for k in strong:
        inner = sum(1 for j in weak if j != k and inside_frac(b[j], b[k]) > 0.8 and box_area(b[j]) < 0.6 * box_area(b[k]))
        if inner >= cfg["cluster_n"]:
            n_cluster += 1
        else:
            singles.append(k)
    if singles:
        best = max(s[k] for k in singles)
        singles = sorted((k for k in singles if s[k] >= cfg["rel_score"] * best), key=lambda k: -s[k])
    kept = []
    for k in singles:
        if all(box_iou(b[k], b[j]) < cfg["nms_iou"] for j in kept):
            kept.append(k)
        if len(kept) == cfg["max_fruits"]:
            break
    return np.array(kept, int), n_cluster


def display_box(b, s, w, h, cfg):
    """Box around the best confident box and the confident boxes whose centres lie within display_reach box sizes of
    it; None when nothing is confident. Also returns the number of confident boxes and whether the display touches
    the frame."""
    idx = confident(b, s, cfg)
    if len(idx) == 0:
        return None, 0, False
    k = idx[0]
    size = max(b[k, 2] - b[k, 0], b[k, 3] - b[k, 1])
    c0 = np.array([(b[k, 0] + b[k, 2]) / 2, (b[k, 1] + b[k, 3]) / 2])
    near = [j for j in idx if np.hypot(*(np.array([(b[j, 0] + b[j, 2]) / 2, (b[j, 1] + b[j, 3]) / 2]) - c0))
            <= cfg["display_reach"] * size]
    d = [float(b[near, 0].min()), float(b[near, 1].min()), float(b[near, 2].max()), float(b[near, 3].max())]
    tol = cfg["edge_tol"]
    cut = bool(d[0] <= tol * w or d[1] <= tol * h or d[2] >= (1 - tol) * w or d[3] >= (1 - tol) * h)
    return d, int(len(idx)), cut


def square_crop(img, box, cfg):
    """Square crop around the box with a margin, shifted inside the photo where possible; long side <= crop_max_side."""
    H, W = img.shape[:2]
    cx, cy = (box[0] + box[2]) / 2, (box[1] + box[3]) / 2
    side = max(box[2] - box[0], box[3] - box[1]) * (1 + 2 * cfg["crop_margin"])
    side = min(side, max(H, W))
    x0 = int(round(min(max(0, cx - side / 2), max(0, W - side))))
    y0 = int(round(min(max(0, cy - side / 2), max(0, H - side))))
    x1, y1 = int(round(min(W, x0 + side))), int(round(min(H, y0 + side)))
    crop = img[y0:y1, x0:x1]
    s = min(1.0, cfg["crop_max_side"] / max(crop.shape[:2]))
    if s < 1:
        crop = cv2.resize(crop, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
    return crop, (x0, y0, x1, y1), s


def clean(m, box=None, pad=0.05):
    """Identical post-processing for every mask: clip to box (+pad), largest component, holes filled."""
    m = m.astype(np.uint8)
    if box is not None:
        x1, y1, x2, y2 = box
        px, py = pad * (x2 - x1), pad * (y2 - y1)
        keep = np.zeros_like(m)
        keep[max(0, int(y1 - py)):int(y2 + py) + 1, max(0, int(x1 - px)):int(x2 + px) + 1] = 1
        m &= keep
    n, lab, st, _ = cv2.connectedComponentsWithStats(m, 8)
    if n <= 1:
        return np.zeros(m.shape, bool)
    m = (lab == 1 + np.argmax(st[1:, cv2.CC_STAT_AREA])).astype(np.uint8)
    cn, _ = cv2.findContours(m, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    out = np.zeros_like(m)
    cv2.drawContours(out, cn, -1, 1, -1)
    return out.astype(bool)


def segment(models, img, boxes):
    """SAM 2.1 with several box prompts on one image embedding -> cleaned masks and SAM scores (same order)."""
    h, w = img.shape[:2]
    if not boxes:
        return [], []
    r = models.sam(cv2.cvtColor(img, cv2.COLOR_RGB2BGR), bboxes=[list(map(float, b)) for b in boxes], verbose=False)[0]
    masks, conf = [np.zeros((h, w), bool) for _ in boxes], [0.0] * len(boxes)
    if r.masks is not None and len(r.masks.data):
        for d, c, s in zip(r.masks.data.cpu().numpy(), r.boxes.cls.cpu().numpy().astype(int), r.boxes.conf.cpu().numpy()):
            masks[c] = cv2.resize(d.astype(np.uint8), (w, h), interpolation=cv2.INTER_NEAREST).astype(bool)
            conf[c] = float(s)
    return [clean(m, b) for m, b in zip(masks, boxes)], conf


def fruit_qc(m, box, cfg):
    """Per-fruit outline checks: pixels, share of the box filled, compactness (solidity) and contact with the crop
    border. Returns (passed, metrics)."""
    n = int(m.sum())
    if n == 0:
        return False, dict(px=0, fill=0.0, solidity=np.nan, border=np.nan)
    cn, _ = cv2.findContours(m.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    c = max(cn, key=cv2.contourArea)
    hull = cv2.contourArea(cv2.convexHull(c))
    sol = float(n / hull) if hull else np.nan
    fill = n / max(1.0, box_area(box))
    border = float(np.concatenate([m[0], m[-1], m[:, 0], m[:, -1]]).mean())
    ok = n >= cfg["min_fruit_px"] and fill >= cfg["min_fill"] and (sol >= cfg["min_solidity"]) and border <= cfg["max_border"]
    return bool(ok), dict(px=n, fill=round(float(fill), 4), solidity=round(sol, 4), border=round(border, 4))


def quality(img, m):
    """Photo and outline quality covariates (not traits)."""
    if not m.any():
        return {}
    pix = img[m]
    inner = cv2.erode(m.astype(np.uint8), np.ones((3, 3), np.uint8)).astype(bool)
    lap = cv2.Laplacian(cv2.cvtColor(img, cv2.COLOR_RGB2GRAY), cv2.CV_32F)
    return dict(clip_hi=float((pix.max(1) >= 250).mean()), clip_lo=float((pix.max(1) <= 5).mean()),
                sharp=float(lap[inner].var()) if inner.sum() > 30 else np.nan)


def fruit_colours(lab, masks):
    """Per-fruit CIELAB means (brightest 2% of pixels, mostly specular highlights, left out) and colour names, one
    row per mask (NaN and '' for masks with fewer than 10 pixels)."""
    V = np.full((len(masks), 3), np.nan)
    L_all = lab[..., 0]
    for i, m in enumerate(masks):
        if m.sum() < 10:
            continue
        L = L_all[m]
        hi = np.percentile(L, 98)
        use = m & (L_all <= hi) if (L <= hi).sum() >= 10 else m
        V[i] = lab[use].mean(0)
    ok = ~np.isnan(V[:, 0])
    names = [""] * len(masks)
    if ok.any():
        for i, n in zip(np.where(ok)[0], np.atleast_1d(colour_name(V[ok, 0], V[ok, 1], V[ok, 2]))):
            names[i] = str(n)
    return V, names


def fruit_summary(V, names):
    """Summaries over fruits: medians, spread, largest colour difference between two fruits, name shares."""
    keep = ~np.isnan(V[:, 0]) if len(V) else np.zeros(0, bool)
    V, names = V[keep], [n for n, k in zip(names, keep) if k]
    if len(V) == 0:
        return {}
    L, a, b = V[:, 0], V[:, 1], V[:, 2]
    C, th = np.hypot(a, b), np.arctan2(b, a)
    hx, hy = (C * np.cos(th)).sum() / (C.sum() + 1e-6), (C * np.sin(th)).sum() / (C.sum() + 1e-6)
    out = dict(n_fruits=int(len(V)), fruit_L_med=float(np.median(L)), fruit_a_med=float(np.median(a)),
               fruit_b_med=float(np.median(b)), fruit_C_med=float(np.median(C)),
               fruit_hue_deg=float(np.degrees(np.arctan2(hy, hx)) % 360),
               fruit_L_iqr=float(np.subtract(*np.percentile(L, [75, 25]))) if len(V) > 1 else 0.0,
               fruit_dE_max=float(ciede2000(V[:, None, :], V[None, :, :]).max()) if len(V) > 1 else 0.0)
    out["fruit_name_med"] = str(colour_name(out["fruit_L_med"], out["fruit_a_med"], out["fruit_b_med"]))
    vals, cnt = np.unique(names, return_counts=True)
    out["fruit_name_major"] = str(vals[np.argmax(cnt)])
    out["fruit_name_share"] = float(cnt.max() / cnt.sum())
    out["fruit_names"] = ";".join(names)
    return out


def ring_contrast(lab, union, others, D):
    """Colour of the surroundings: ring of width 0.5 x the median fruit diameter around the fruit mask (other fruit
    pixels excluded); share of green vegetation in the ring, CIEDE2000 and lightness difference to the fruit."""
    r = int(np.clip(round(0.5 * D), 3, 60))
    k = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (2 * r + 1, 2 * r + 1))
    ring = cv2.dilate(union.astype(np.uint8), k).astype(bool) & ~union & ~others
    if ring.sum() < 50:
        return dict(ring_px=int(ring.sum()), ring_L=np.nan, ring_a=np.nan, ring_b=np.nan, ring_green_share=np.nan,
                    ring_dE00=np.nan, ring_dL=np.nan)
    rl = lab[ring]
    fv = lab[union].mean(0)
    rv = np.median(rl, 0)
    h = np.degrees(np.arctan2(rl[:, 2], rl[:, 1])) % 360
    green = (np.hypot(rl[:, 1], rl[:, 2]) >= 8) & (h >= 95) & (h < 200)
    return dict(ring_px=int(ring.sum()), ring_L=float(rv[0]), ring_a=float(rv[1]), ring_b=float(rv[2]),
                ring_green_share=float(green.mean()), ring_dE00=float(ciede2000(fv, rv)), ring_dL=float(fv[0] - rv[0]))


def lab_image(img):
    """CIELAB (D65) of an sRGB uint8 image through linear RGB (as in colour_metrics)."""
    x = img.astype(np.float32) / 255
    lin = np.where(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055) ** 2.4).astype(np.float32)
    return cv2.cvtColor(lin, cv2.COLOR_LRGB2Lab)


def process_photo(models, photo, two_stage=None):
    """photo: RGB array (full photo). Returns (record dict, crop RGB, fruit mask bool, list of per-fruit masks)."""
    cfg = models.cfg
    two_stage = cfg["two_stage"] if two_stage is None else two_stage
    H, W = photo.shape[:2]
    rec = dict(photo_w=W, photo_h=H)
    (b1, s1), = detect(models, [photo], cfg["stage1_size"])
    disp, n_conf1, cut = display_box(b1, s1, W, H, cfg)
    rec.update(det1_score=round(float(s1.max()) if len(s1) else 0.0, 4), n_det1=int(len(s1)), n_conf1=n_conf1,
               cut_by_frame=cut)
    if disp is None:
        disp = [0.03 * W, 0.03 * H, 0.97 * W, 0.97 * H]
    crop, (x0, y0, x1, y1), s = square_crop(photo, disp, cfg)
    h, w = crop.shape[:2]
    rec.update(crop_x0=x0, crop_y0=y0, crop_x1=x1, crop_y1=y1, crop_scale=round(s, 4), W=w, H=h,
               display_frac=round(box_area(disp) / (W * H), 4))
    if two_stage:
        (b, sc), = detect(models, [crop], cfg["stage2_size"])
    else:  # stage-1 boxes mapped into the crop
        b = np.c_[(b1[:, 0] - x0) * s, (b1[:, 1] - y0) * s, (b1[:, 2] - x0) * s, (b1[:, 3] - y0) * s]
        b[:, [0, 2]] = b[:, [0, 2]].clip(0, w)
        b[:, [1, 3]] = b[:, [1, 3]].clip(0, h)
        sc = s1
    keep, n_cluster = single_fruit_boxes(b, sc, cfg)
    rec.update(det_score=round(float(sc[keep[0]]) if len(keep) else 0.0, 4), n_det=int(len(sc)),
               n_cluster_boxes=int(n_cluster), n_fruit_boxes=int(len(keep)))
    boxes = [[float(v) for v in b[k]] for k in keep]
    if not boxes:
        rec.update(n_fruits_ok=0, mask_ok=False)
        return rec, crop, np.zeros((h, w), bool), []
    prompts = boxes + [scale_box(boxes[0], f, w, h) for f in cfg["jitter"]]
    ms, conf = segment(models, crop, prompts)
    fruit_ms, jit = ms[:len(boxes)], ms[len(boxes):]
    rec["stab"] = round(min(mask_iou(fruit_ms[0], x) for x in jit), 4) if fruit_ms[0].any() and jit else 0.0
    ok_ms, qcs = [], []
    for i, (m, bx, c) in enumerate(zip(fruit_ms, boxes, conf)):
        ok, q = fruit_qc(m, bx, cfg)
        others_i = np.zeros((h, w), bool)
        for j, mj in enumerate(fruit_ms):
            if j != i:
                others_i |= mj
        q["overlap"] = round(float((m & others_i).sum() / max(1, m.sum())), 4)
        ok = ok and q["overlap"] <= cfg["max_overlap"]
        fails = [k for k, bad in (("small", q["px"] < cfg["min_fruit_px"]), ("fill", q["fill"] < cfg["min_fill"]),
                                  ("shape", not q["solidity"] >= cfg["min_solidity"]),
                                  ("border", not q["border"] <= cfg["max_border"]),
                                  ("overlap", q["overlap"] > cfg["max_overlap"])) if bad]
        qcs.append({**q, "fruit": i + 1, "x0": round(bx[0], 1), "y0": round(bx[1], 1), "x1": round(bx[2], 1),
                    "y1": round(bx[3], 1), "det": round(float(sc[keep[i]]), 4), "sam": round(float(c), 4), "ok": ok,
                    "fail": ";".join(fails)})
        if ok:
            ok_ms.append(m)
    ok_i = [i for i, q in enumerate(qcs) if q["ok"]]
    if len(ok_i) >= 3:  # an outlier in size among three or more fruits is an undetected cluster
        px = np.array([qcs[i]["px"] for i in ok_i], float)
        for n, i in enumerate(ok_i):
            if px[n] > cfg["max_size_ratio"] * np.median(np.delete(px, n)):
                qcs[i]["ok"] = False
                qcs[i]["fail"] = ";".join([f for f in (qcs[i]["fail"], "size") if f])
    ok_ms = [m for m, q in zip(fruit_ms, qcs) if q["ok"]]
    for n, q in enumerate([q for q in qcs if q["ok"]], 1):
        q["label"] = n
    union = np.zeros((h, w), bool)
    for m in ok_ms:
        union |= m
    others = np.zeros((h, w), bool)
    for m, q in zip(fruit_ms, qcs):
        if not q["ok"]:
            others |= m
    n_ok = len(ok_ms)
    bt = float(np.concatenate([union[0], union[-1], union[:, 0], union[:, -1]]).mean()) if union.any() else np.nan
    px_ok = [q["px"] for q in qcs if q["ok"]]
    rec.update(n_fruits_ok=n_ok, fruit_px_med=float(np.median(px_ok)) if px_ok else 0.0,
               fruit_px_max=float(max(px_ok)) if px_ok else 0.0,
               n_fruits_small=int(sum(1 for q in qcs if q["px"] < cfg["min_fruit_px"])),
               n_fruits_overlapping=int(sum(1 for q in qcs if q["overlap"] > cfg["max_overlap"])),
               fruit_fill_med=float(np.median([q["fill"] for q in qcs if q["ok"]])) if n_ok else np.nan,
               fruit_solidity_med=float(np.median([q["solidity"] for q in qcs if q["ok"]])) if n_ok else np.nan,
               sam_score_med=float(np.median([q["sam"] for q in qcs if q["ok"]])) if n_ok else np.nan,
               union_px=int(union.sum()), mask_frac=round(float(union.mean()), 4), border_touch=bt,
               mask_ok=bool(n_ok >= 1 and union.sum() >= cfg["min_union_px"] and (bt <= cfg["max_border"])))
    rec["fruits"] = qcs
    if not union.any():
        return rec, crop, union, ok_ms
    rec.update(quality(crop, union))
    lab = lab_image(crop)
    D = 2 * np.sqrt(np.median(px_ok) / np.pi)
    V, names = fruit_colours(lab, ok_ms)
    rec.update(fruit_summary(V, names))
    lin = (crop.astype(np.float32) / 255)
    lin = np.where(lin <= 0.04045, lin / 12.92, ((lin + 0.055) / 1.055) ** 2.4).astype(np.float32)
    ill = estimate_illuminant(lin, "gray_world")
    rec.update(illum_r=float(ill[0]), illum_g=float(ill[1]), illum_b=float(ill[2]))
    lab_wb = cv2.cvtColor(np.clip(lin / ill[None, None, :], 0, 1).astype(np.float32), cv2.COLOR_LRGB2Lab)
    V_wb, _ = fruit_colours(lab_wb, ok_ms)
    for q, v, vw, n, m in zip([q for q in qcs if q["ok"]], V, V_wb, names, ok_ms):
        cn, _ = cv2.findContours(m.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
        (_, _), (rw, rh), _ = cv2.minAreaRect(max(cn, key=cv2.contourArea))
        q.update(wb_L=float(vw[0]), wb_a=float(vw[1]), wb_b=float(vw[2]))
        q.update(L=float(v[0]), a=float(v[1]), b=float(v[2]), C=float(np.hypot(v[1], v[2])),
                 hue_deg=float(np.degrees(np.arctan2(v[2], v[1])) % 360), name=n,
                 diam_px_photo=float(2 * np.sqrt(q["px"] / np.pi) / s), elongation=float(max(rw, rh) / max(1e-6, min(rw, rh))))
    rec.update(ring_contrast(lab, union, others, D))
    col, _ = colour_metrics(crop, union)
    rec.update({k: col[k] for k in ("gloss_frac", "clip_frac", "L_mean", "L_median", "L_sd", "a_mean", "b_mean", "C_mean",
                                    "hue_deg", "hue_concentration", "dark_frac_L30", "n_colour_classes",
                                    "colour_contrast_dE", "dominant_frac", "dominant_L", "dominant_C", "dominant_hue",
                                    "dominant_name")})
    top = ok_ms[0] if ok_ms else union
    cn, _ = cv2.findContours(top.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    if cn:
        (_, _), (rw, rh), _ = cv2.minAreaRect(max(cn, key=cv2.contourArea))
        rec["fruit_elongation"] = float(max(rw, rh) / max(1e-6, min(rw, rh)))
    return rec, crop, union, ok_ms
