"""Photo and fruit filters: is each outlined object a single fleshy fruit with a correct outline, and is it ripe?

Heads (small logistic regressions) are trained on visually labelled outlines of iNaturalist photographs of fruiting
plants (train_heads.py). Photo features: OpenCLIP ViT-B/16 and DINOv2-small embeddings of the crop and of the
fruit-only crop (grey background), zero-shot text similarities and outline and quality metrics from
photo.process_photo. Fruit features (one row per fruit that passed the outline checks): OpenCLIP embeddings of the
fruit in context (box + 25%) and of the fruit alone (grey background), with outline, colour and relative metrics.

    from fruittrait.photo import Models, load_rgb, process_photo
    from fruittrait.photofilter import PhotoFilter
    M, F = Models(), PhotoFilter("filter_heads.json")
    rec, crop, mask, fruits = process_photo(M, load_rgb("photo.jpg", 2048))
    rec.update(F.predict(crop, mask, rec))
    rec.update(F.decide(rec))
"""
import json

import numpy as np
import torch
from PIL import Image

PROMPTS = {
    "ripe": ["ripe fruits", "fully ripe berries"],
    "unripe": ["unripe green fruits", "immature fruits"],
    "dry": ["a dry seed pod", "a dry capsule with seeds"],
    "fleshy": ["a fleshy berry", "juicy fruits"],
    "flower": ["a flower in bloom", "flowers"],
    "leaves": ["green leaves without fruits", "a plant without fruits"],
    "hand": ["fruits held in a hand", "a hand holding a fruit"],
    "ground": ["fruits lying on the ground", "fallen fruits on the soil"],
    "cut": ["a fruit cut open", "a section through a fruit"],
    "cluster": ["a cluster of many small berries", "a bunch of fruits"],
    "single": ["a single large fruit"],
    "sharp": ["a sharp, in-focus photo of a fruit"],
    "blurry": ["a blurry, out-of-focus photo"],
    "dark": ["a dark, underexposed photo"],
}
QC_FEATURES = ["det1_score", "det_score", "n_conf1", "n_det", "n_cluster_boxes", "n_fruit_boxes", "n_fruits_ok",
               "cut_by_frame", "stab", "mask_frac", "border_touch", "fruit_fill_med", "fruit_solidity_med",
               "sam_score_med", "clip_hi", "clip_lo", "display_frac", "log_sharp", "log_px", "fruit_L_iqr",
               "fruit_dE_max", "ring_green_share", "gloss_frac"] + [f"zs_{k}" for k in PROMPTS]
DEFAULT_THRESHOLDS = dict(p_usable=0.5, p_fruit=0.5, p_outline_ok=0.5)
FRUIT_FEATURES = ["log_px", "fill", "solidity", "border", "overlap", "det", "sam", "L", "a", "b", "C", "hue_sin",
                  "hue_cos", "elongation", "rel_log_px", "rel_L", "rel_C"]


class PhotoFilter:
    def __init__(self, heads_json=None, device=None, clip_arch="ViT-B-16", clip_pretrained="datacomp_xl_s13b_b90k",
                 dino="facebook/dinov2-small"):
        import open_clip
        from transformers import AutoImageProcessor, AutoModel
        self.spec = json.load(open(heads_json)) if heads_json else dict(
            clip_arch=clip_arch, clip_pretrained=clip_pretrained, dino=dino, prompts=PROMPTS, qc_features=QC_FEATURES,
            heads={})
        self.device = device or ("cuda" if torch.cuda.is_available() else "cpu")
        self.clip, _, self.pre = open_clip.create_model_and_transforms(self.spec["clip_arch"],
                                                                       pretrained=self.spec["clip_pretrained"])
        self.clip = self.clip.to(self.device).eval()
        tok = open_clip.get_tokenizer(self.spec["clip_arch"])
        with torch.no_grad():
            T = []
            for ps in self.spec["prompts"].values():
                e = self.clip.encode_text(tok(ps).to(self.device))
                e = (e / e.norm(dim=-1, keepdim=True)).mean(0)
                T.append((e / e.norm()).cpu().numpy())
        self.text = np.stack(T, 1)
        if self.spec.get("prompt_text_emb") is not None:  # the stored matrix used to train the fruit heads
            self.text = np.asarray(self.spec["prompt_text_emb"], np.float32)
        self.dproc = self.dino = None
        if self.spec.get("dino"):
            self.dproc = AutoImageProcessor.from_pretrained(self.spec["dino"])
            self.dino = AutoModel.from_pretrained(self.spec["dino"]).to(self.device).eval()

    @staticmethod
    def masked_crop(crop, mask):
        g = crop.copy()
        g[~mask] = 128
        ys, xs = np.nonzero(mask)
        if len(ys):
            pad = int(0.05 * max(np.ptp(ys), np.ptp(xs))) + 2
            g = g[max(0, ys.min() - pad):ys.max() + pad + 1, max(0, xs.min() - pad):xs.max() + pad + 1]
        return g

    @torch.no_grad()
    def _embed(self, rgb):
        pil = Image.fromarray(rgb)
        c = self.clip.encode_image(self.pre(pil)[None].to(self.device))
        c = (c / c.norm(dim=-1, keepdim=True)).cpu().numpy()[0]
        if self.dino is None:
            return c, np.zeros(0, np.float32)
        o = self.dino(**self.dproc(images=[pil], return_tensors="pt").to(self.device))
        d = torch.cat([o.last_hidden_state[:, 0], o.last_hidden_state[:, 1:].mean(1)], 1)
        d = (d / d.norm(dim=-1, keepdim=True)).cpu().numpy()[0]
        return c, d

    def _qc_vector(self, rec, zs):
        q = {k: rec.get(k, np.nan) for k in self.spec["qc_features"]}
        q["log_sharp"] = np.log1p(rec.get("sharp", np.nan))
        q["log_px"] = np.log(max(1.0, rec.get("fruit_px_med", np.nan) or 1.0))
        q["cut_by_frame"] = float(bool(rec.get("cut_by_frame", False)))
        for k, v in zip(self.spec["prompts"], zs):
            q["zs_" + k] = v
        x = np.array([float(q.get(k, np.nan)) for k in self.spec["qc_features"]], float)
        med = np.array(self.spec.get("qc_medians", [0.0] * len(x)), float)
        x[np.isnan(x)] = med[np.isnan(x)]
        return x

    @torch.no_grad()
    def _clip_batch(self, imgs):
        x = torch.stack([self.pre(Image.fromarray(i)) for i in imgs]).to(self.device)
        c = self.clip.encode_image(x)
        return (c / c.norm(dim=-1, keepdim=True)).cpu().numpy()

    @staticmethod
    def fruit_crops(crop, fruit_masks, fruits_qc, margin=0.25):
        """Per passing fruit: the fruit in context (box + margin) and the fruit alone on grey."""
        ctx, alone = [], []
        h, w = crop.shape[:2]
        for q, m in zip([q for q in fruits_qc if q["ok"]], fruit_masks):
            bw, bh = q["x1"] - q["x0"], q["y1"] - q["y0"]
            x0, y0 = int(max(0, q["x0"] - margin * bw)), int(max(0, q["y0"] - margin * bh))
            x1, y1 = int(min(w, q["x1"] + margin * bw)) + 1, int(min(h, q["y1"] + margin * bh)) + 1
            ctx.append(crop[y0:y1, x0:x1])
            alone.append(PhotoFilter.masked_crop(crop, m))
        return ctx, alone

    @staticmethod
    def fruit_qc_matrix(fruits_qc):
        ok = [q for q in fruits_qc if q["ok"]]
        if not ok:
            return np.zeros((0, len(FRUIT_FEATURES)))
        lpx = np.log(np.array([q["px"] for q in ok], float))
        L = np.array([q.get("L", np.nan) for q in ok], float)
        C = np.array([q.get("C", np.nan) for q in ok], float)
        rows = []
        for q, lp, l, c in zip(ok, lpx, L, C):
            hr = np.radians(q.get("hue_deg", np.nan))
            rows.append([lp, q["fill"], q["solidity"], q["border"], q["overlap"], q["det"], q["sam"], l, q.get("a", np.nan),
                         q.get("b", np.nan), c, np.sin(hr), np.cos(hr), q.get("elongation", np.nan),
                         lp - np.median(lpx), l - np.nanmedian(L), c - np.nanmedian(C)])
        return np.array(rows, float)

    def fruit_features(self, crop, fruit_masks, fruits_qc):
        """Feature sets of the fruit heads, one row per fruit that passed the outline checks (same order as the
        fruit label map)."""
        if not fruit_masks:
            z = np.zeros((0, 512), np.float32)
            return {"fruit_clip_ctx": z, "fruit_clip_alone": z, "fruit_qc": self.fruit_qc_matrix([]),
                    "fruit_zs": np.zeros((0, 2 * self.text.shape[1]), np.float32)}
        ctx, alone = self.fruit_crops(crop, fruit_masks, fruits_qc)
        e = self._clip_batch(ctx + alone)
        n = len(ctx)
        return {"fruit_clip_ctx": e[:n], "fruit_clip_alone": e[n:], "fruit_qc": self.fruit_qc_matrix(fruits_qc),
                "fruit_zs": fruit_zeroshot(e[:n], e[n:], self.text).astype(np.float32)}

    def features(self, crop, mask, rec):
        """Feature sets used by the heads (also saved for re-training)."""
        cf, df = self._embed(crop)
        cm, dm = self._embed(self.masked_crop(crop, mask)) if mask.any() else (cf, df)
        zs = cf @ self.text
        q = self._qc_vector(rec, zs)
        return {"QC": q, "clip": cf, "dino": df, "clip+dino": np.r_[cf, df], "clip+dino(masked)": np.r_[cm, dm],
                "clip+dino+QC": np.r_[cf, df, q], "zeroshot": zs}

    def heads_from_features(self, sets):
        out = {}
        for t, h in self.spec.get("heads", {}).items():
            x = (sets[h["features"]] - np.array(h["mean"])) / np.array(h["scale"])
            out[f"p_{t}"] = float(1 / (1 + np.exp(-(x @ np.array(h["coef"]) + h["intercept"]))))
        return out

    def predict(self, crop, mask, rec):
        """Probabilities of every head in the heads file (p_usable, p_fruit, p_ripe, p_attached, p_fleshy,
        p_outline_ok)."""
        return self.heads_from_features(self.features(crop, mask, rec))

    @staticmethod
    def decide(rec, thresholds=None):
        """Keep or reject with a reason (default rule)."""
        th = {**DEFAULT_THRESHOLDS, **(thresholds or {})}
        if not rec.get("mask_ok", False):
            return dict(keep=False, reason="no fruit outlined")
        if rec.get("p_fruit", 1) < th["p_fruit"]:
            return dict(keep=False, reason="no fruit visible")
        if rec.get("p_outline_ok", 1) < th["p_outline_ok"]:
            return dict(keep=False, reason="outline wrong")
        if rec.get("p_usable", 1) < th["p_usable"]:
            return dict(keep=False, reason="unclear photo (blurry, dark or fruits hidden)")
        return dict(keep=True, reason="kept")


FRUIT_SETS = {"fruit_qc": ("fruit_qc",), "clip_ctx": ("fruit_clip_ctx",), "clip_alone": ("fruit_clip_alone",),
              "clip_ctx+alone": ("fruit_clip_ctx", "fruit_clip_alone"),
              "clip_alone+qc": ("fruit_clip_alone", "fruit_qc"),
              "clip_ctx+alone+qc": ("fruit_clip_ctx", "fruit_clip_alone", "fruit_qc"),
              "zs": ("fruit_zs",), "zs+qc": ("fruit_zs", "fruit_qc"),
              "clip_ctx+alone+zs": ("fruit_clip_ctx", "fruit_clip_alone", "fruit_zs"),
              "clip_ctx+alone+qc+zs": ("fruit_clip_ctx", "fruit_clip_alone", "fruit_qc", "fruit_zs")}


def fruit_zeroshot(ctx, alone, text):
    """Zero-shot text similarities of the fruit embeddings: cosine similarity of the fruit in context and of the
    fruit alone with each prompt class (columns of text, unit vectors), one row per fruit."""
    ctx, alone, text = (np.asarray(v, float) for v in (ctx, alone, text))
    if len(ctx) == 0:
        return np.zeros((0, 2 * text.shape[1]))
    nc = ctx / np.linalg.norm(ctx, axis=1, keepdims=True)
    na = alone / np.linalg.norm(alone, axis=1, keepdims=True)
    return np.hstack([nc @ text, na @ text])


def fruit_set(feats, name):
    """Feature matrix of a named fruit feature set (rows = fruits that passed the outline checks)."""
    return np.hstack([np.asarray(feats[k], float) for k in FRUIT_SETS[name]])


def fruit_head_probs(feats, spec):
    """Probabilities of the fruit heads of a heads file (spec['fruit_heads']), one array per head."""
    out = {}
    for name, h in spec.get("fruit_heads", {}).items():
        x = fruit_set(feats, h["features"])
        if len(x) == 0:
            out[name] = np.zeros(0)
            continue
        med = np.asarray(h.get("medians", np.zeros(x.shape[1])), float)
        x = np.where(np.isnan(x), med[None, :], x)
        z = (x - np.asarray(h["mean"])) / np.asarray(h["scale"])
        out[name] = 1 / (1 + np.exp(-(z @ np.asarray(h["coef"]) + h["intercept"])))
    return out


def apply_fruit_heads(fruits, probs, thresholds):
    """Adds p_<head> to the fruits that passed the outline checks and the decision 'measurable' (single fruit with a
    correct outline and ripe: p_valid and p_ripe at least their thresholds); NaN without heads."""
    ok = [q for q in fruits if q.get("ok")]
    for name, p in probs.items():
        for q, v in zip(ok, p):
            q[f"p_{name}"] = float(v)
    for q in ok:
        if "p_valid" in q and "p_ripe" in q:
            q["valid"] = q["p_valid"] >= thresholds.get("valid", 0.5)
            q["measurable"] = bool(q["valid"] and q["p_ripe"] >= thresholds.get("ripe", 0.5))
        if "p_dry" in q:
            q["dry"] = q["p_dry"] >= thresholds.get("dry", 0.5)
    return fruits


def summarise_fruits(fruits, prefix, select):
    """Colour of the selected fruits of a photo: CIELAB mean of the fruit means (each fruit weighted equally), chroma
    and hue of the mean, colour name, spread of lightness, grey-world white-balanced mean, number of fruits."""
    from .colour import colour_name
    sel = [q for q in fruits if q.get("ok") and select(q) and q.get("L") is not None and np.isfinite(q["L"])]
    out = {f"{prefix}_n": len(sel)}
    if not sel:
        return out
    V = np.array([[q["L"], q["a"], q["b"]] for q in sel], float)
    L, a, b = V.mean(0)
    out.update({f"{prefix}_L": L, f"{prefix}_a": a, f"{prefix}_b": b, f"{prefix}_C": float(np.hypot(a, b)),
                f"{prefix}_hue": float(np.degrees(np.arctan2(b, a)) % 360),
                f"{prefix}_name": str(colour_name(L, a, b)), f"{prefix}_L_sd": float(V[:, 0].std()) if len(V) > 1 else 0.0,
                f"{prefix}_diam_px": float(np.median([q["diam_px_photo"] for q in sel]))})
    W = np.array([[q.get("wb_L", np.nan), q.get("wb_a", np.nan), q.get("wb_b", np.nan)] for q in sel], float)
    if np.isfinite(W).all():
        out.update({f"{prefix}_wb_L": float(W[:, 0].mean()), f"{prefix}_wb_a": float(W[:, 1].mean()),
                    f"{prefix}_wb_b": float(W[:, 2].mean())})
    return out


def write_prompt_text_emb(base_json, out_json=None):
    """Stores the unit text embeddings of the prompt classes (columns, in the order of 'prompts') in a heads file, so
    that the fruit zero-shot features can be computed from saved fruit embeddings without the text encoder."""
    import open_clip
    spec = json.load(open(base_json))
    model, _, _ = open_clip.create_model_and_transforms(spec["clip_arch"], pretrained=spec["clip_pretrained"])
    tok = open_clip.get_tokenizer(spec["clip_arch"])
    model.eval()
    with torch.no_grad():
        T = []
        for ps in spec["prompts"].values():
            e = model.encode_text(tok(ps))
            e = (e / e.norm(dim=-1, keepdim=True)).mean(0)
            T.append((e / e.norm()).numpy())
    spec["prompt_text_emb"] = np.round(np.stack(T, 1), 7).tolist()
    json.dump(spec, open(out_json or base_json, "w"), indent=1)
    return spec


if __name__ == "__main__":
    import sys
    if len(sys.argv) >= 3 and sys.argv[1] == "--write-text-emb":
        write_prompt_text_emb(sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else None)
