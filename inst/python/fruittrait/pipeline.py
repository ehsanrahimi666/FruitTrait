"""Full analysis of one photo: fruit outlines and colours (photo.py), fruit features and fruit heads (photofilter.py),
photo summaries and fruit rows in the coordinates of the original photo.

Used by the batch runner (run_photos.py), by the GPU worker of the data set and by the R package, so that all of them
give the same columns.

    from fruittrait.photo import Models
    from fruittrait.photofilter import PhotoFilter
    from fruittrait.pipeline import analyse
    M, F = Models(), PhotoFilter("heads_fruitdata.json")
    out = analyse(M, F, img, photo_scale=1.0)
    out["rec"], out["fruit_rows"]
"""
import time

import cv2
import numpy as np

from . import photo as P
from .photofilter import apply_fruit_heads, fruit_head_probs, summarise_fruits

FRUIT_COLS = ["photo_id", "fruit", "label", "ok", "fail", "x0_photo", "y0_photo", "x1_photo", "y1_photo", "px", "fill",
              "solidity", "border", "overlap", "det", "sam", "diam_px_photo", "elongation", "L", "a", "b", "C", "hue_deg",
              "name", "wb_L", "wb_a", "wb_b", "p_valid", "p_ripe", "p_dry", "valid", "measurable", "dry", "outline_xy"]


def to_photo(x, y, rec, photo_scale):
    """Crop pixel coordinates -> pixel coordinates of the original photo (before any reduction)."""
    s = rec.get("crop_scale", 1.0) or 1.0
    return ((rec["crop_x0"] + np.asarray(x, float) / s) / photo_scale,
            (rec["crop_y0"] + np.asarray(y, float) / s) / photo_scale)


def outline_polygon(mask, rec, photo_scale):
    """Outline as 'x,y;x,y;...' in pixel coordinates of the original photo (approxPolyDP, 0.5% of perimeter)."""
    cn, _ = cv2.findContours(mask.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    if not cn:
        return ""
    c = max(cn, key=cv2.contourArea)
    ap = cv2.approxPolyDP(c, 0.005 * cv2.arcLength(c, True), True).reshape(-1, 2).astype(float)
    xs, ys = to_photo(ap[:, 0], ap[:, 1], rec, photo_scale)
    return ";".join(f"{int(round(x))},{int(round(y))}" for x, y in zip(xs, ys))


def analyse(models, filt, img, photo_scale=1.0, spec=None, sync=None):
    """img: RGB photo as processed (long side <= photo_max_side); photo_scale: processed / original size.
    Returns a dict with rec (photo record), fruit_rows (one per detected fruit), crop, union (mask of the fruits that
    passed the outline checks), ok_masks, features (fruit feature sets) and clip_crop (photo embedding)."""
    spec = filt.spec if spec is None else spec
    heads = bool(spec.get("fruit_heads"))
    t0 = time.perf_counter()
    res, crop, union, ok_ms = P.process_photo(models, img)
    t_pp = time.perf_counter() - t0
    fruits = res.pop("fruits", []) or []
    t1 = time.perf_counter()
    cf, _ = filt._embed(crop)
    feats = filt.fruit_features(crop, ok_ms, fruits)
    if sync:
        sync()
    t_embed = time.perf_counter() - t1
    t2 = time.perf_counter()
    apply_fruit_heads(fruits, fruit_head_probs(feats, spec), spec.get("thresholds", {}))
    res.update(summarise_fruits(fruits, "m", lambda q: q.get("measurable") is True))
    res.update(summarise_fruits(fruits, "v", lambda q: q.get("valid") is True))
    res["measurable"] = bool(res.get("m_n", 0) >= 1) if heads else np.nan
    res["n_valid_dry"] = int(sum(1 for q in fruits if q.get("valid") is True and q.get("dry") is True))
    okq = [q for q in fruits if q.get("ok")]
    for q, m in zip(okq, ok_ms):
        q["outline_xy"] = outline_polygon(m, res, photo_scale)
    for q in fruits:
        x0, y0 = to_photo(q["x0"], q["y0"], res, photo_scale)
        x1, y1 = to_photo(q["x1"], q["y1"], res, photo_scale)
        q.update(x0_photo=round(float(x0), 1), y0_photo=round(float(y0), 1), x1_photo=round(float(x1), 1),
                 y1_photo=round(float(y1), 1))
    rows = [{k: q.get(k) for k in FRUIT_COLS if k != "photo_id"} for q in fruits]
    t_heads = time.perf_counter() - t2
    return dict(rec=res, fruit_rows=rows, fruits=fruits, crop=crop, union=union, ok_masks=ok_ms, features=feats,
                clip_crop=cf, t_pp=t_pp, t_embed=t_embed, t_heads=t_heads)


def read_photo(path_or_bytes, max_side=2048):
    """Photo as RGB uint8 reduced to <= max_side (area averaging) and the reduction factor; None if unreadable."""
    buf = np.frombuffer(path_or_bytes, np.uint8) if isinstance(path_or_bytes, (bytes, bytearray)) else \
        np.fromfile(str(path_or_bytes), np.uint8)
    bgr = cv2.imdecode(buf, cv2.IMREAD_COLOR) if buf.size else None
    if bgr is None:
        return None, None, None
    rgb = cv2.cvtColor(bgr, cv2.COLOR_BGR2RGB)
    orig = rgb.shape[:2]
    s = max_side / max(orig)
    if s < 1:
        rgb = cv2.resize(rgb, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
    return rgb, (s if s < 1 else 1.0), orig
