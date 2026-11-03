"""Batch processing of fruit photographs (resumable).

python -m fruittrait.run_photos --list photos.csv --out out_dir [--heads heads.json] [--one-stage]
photos.csv: photo_id and either path or photo_url (downloaded to out_dir/photos); other columns are copied to the
output. Every finished photo appends one JSON line to out_dir/records.jsonl (photo record with its fruit rows);
collect() turns the lines into traits.csv (one row per photo) and fruits.csv (one row per detected fruit, with the
outline checks, the box and outline in the original photo, colour and fruit-head probabilities). Images: crops/<id>.jpg,
masks/<id>.png (fruits that passed the outline checks), fruits/<id>.png (label map of those fruits), overlays/<id>.jpg
(measurable fruits outlined in yellow, other passing fruits in white, rejected fruit boxes in cyan) and, with
--embeddings, embeddings/<id>.npz (fruit feature sets, for training new heads).
The heads file defaults to the fruit heads shipped with the package (data/heads_fruitdata.json) when present.
"""
import argparse
import json
import math
import os
import time
import urllib.request

import cv2
import numpy as np
import pandas as pd

from .photo import Models
from .pipeline import FRUIT_COLS, analyse, read_photo

UA = {"User-Agent": "FruitTrait/0.1 (research data set)"}
HERE = os.path.dirname(os.path.abspath(__file__))


def default_heads():
    for f in ("heads_fruitdata.json", "fruit_filter_base.json"):
        p = os.path.join(HERE, "data", f)
        if os.path.exists(p):
            return p
    return None


def fetch(url, dest, tries=4):
    if os.path.exists(dest) and os.path.getsize(dest) > 0:
        return dest
    for i in range(tries):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=120) as r, \
                    open(dest + ".part", "wb") as f:
                f.write(r.read())
            os.replace(dest + ".part", dest)
            return dest
        except Exception:
            time.sleep(3 * (i + 1))
    raise IOError(f"download failed: {url}")


def plain(v):
    """JSON-safe value: numpy scalars to Python, NaN and infinity to None."""
    if isinstance(v, dict):
        return {k: plain(x) for k, x in v.items()}
    if isinstance(v, (list, tuple)):
        return [plain(x) for x in v]
    if isinstance(v, np.generic):
        v = v.item()
    if isinstance(v, float) and not math.isfinite(v):
        return None
    return v


def read_records(out):
    """Records of out_dir/records.jsonl; an incomplete last line (interrupted run) is ignored."""
    path = os.path.join(out, "records.jsonl")
    recs = []
    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            for line in f:
                try:
                    recs.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return recs


def collect(out):
    """records.jsonl -> traits.csv (photos) and fruits.csv (fruits); returns both tables."""
    recs = read_records(out)
    photos = pd.DataFrame([{k: v for k, v in r.items() if k != "fruit_rows"} for r in recs])
    fruits = pd.DataFrame([dict(photo_id=r["photo_id"], **f) for r in recs for f in (r.get("fruit_rows") or [])],
                          columns=FRUIT_COLS)
    if len(photos):
        photos = photos.drop_duplicates("photo_id", keep="last")
        fruits = fruits.drop_duplicates(["photo_id", "fruit"], keep="last")
    photos.to_csv(os.path.join(out, "traits.csv"), index=False)
    fruits.to_csv(os.path.join(out, "fruits.csv"), index=False)
    return photos, fruits


def overlay(crop, fruits, masks):
    """Crop with measurable fruits outlined in yellow, other passing fruits in white, rejected boxes in cyan."""
    o = crop.copy()
    t = max(1, crop.shape[0] // 300)
    for q in fruits:
        if not q.get("ok"):
            cv2.rectangle(o, (int(q["x0"]), int(q["y0"])), (int(q["x1"]), int(q["y1"])), (0, 220, 255), max(1, t // 2))
    for q, m in zip([q for q in fruits if q.get("ok")], masks):
        cn, _ = cv2.findContours(m.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
        cv2.drawContours(o, cn, -1, (255, 230, 0) if q.get("measurable") is True else (255, 255, 255), t)
    return o


def run(table, out, heads=None, two_stage=True, max_side=2048, log_every=10, max_hours=None, embeddings=False):
    """Process the photos of `table` not yet in out_dir/records.jsonl; stops cleanly after max_hours."""
    from .photofilter import PhotoFilter
    for d in ("photos", "crops", "masks", "fruits", "overlays") + (("embeddings",) if embeddings else ()):
        os.makedirs(os.path.join(out, d), exist_ok=True)
    done = {str(r["photo_id"]) for r in read_records(out)}
    todo = table[~table.photo_id.astype(str).isin(done)]
    if todo.empty:
        return collect(out)
    M = Models({"two_stage": two_stage})
    F = PhotoFilter(heads or default_heads())
    t0 = time.time()
    with open(os.path.join(out, "records.jsonl"), "a", encoding="utf-8") as jf:
        for i, r in enumerate(todo.itertuples(index=False), 1):
            pid = str(r.photo_id)
            rec = {c: getattr(r, c) for c in table.columns}
            rec["photo_id"] = pid
            t = time.time()
            try:
                path = getattr(r, "path", None) if "path" in table.columns else None
                if not isinstance(path, str) or not path:
                    path = fetch(r.photo_url, os.path.join(out, "photos", f"{pid}.jpg"))
                img, scale, orig = read_photo(path, max_side)
                if img is None:
                    raise IOError("unreadable image")
                rec.update(orig_w_px=orig[1], orig_h_px=orig[0], photo_scale=scale)
                A = analyse(M, F, img, scale)
                rec.update(A["rec"])
                rec["fruit_rows"] = A["fruit_rows"]
                crop, union, ok_ms = A["crop"], A["union"], A["ok_masks"]
                if embeddings:
                    np.savez_compressed(os.path.join(out, "embeddings", f"{pid}.npz"), clip_crop=A["clip_crop"],
                                        **A["features"])
                cv2.imwrite(os.path.join(out, "crops", f"{pid}.jpg"), cv2.cvtColor(crop, cv2.COLOR_RGB2BGR),
                            [cv2.IMWRITE_JPEG_QUALITY, 92])
                cv2.imwrite(os.path.join(out, "masks", f"{pid}.png"), union.astype(np.uint8) * 255)
                lab = np.zeros(union.shape, np.uint8)
                for q, m in zip([q for q in A["fruits"] if q.get("ok")], ok_ms):
                    lab[m & (lab == 0)] = q["label"]
                cv2.imwrite(os.path.join(out, "fruits", f"{pid}.png"), lab)
                cv2.imwrite(os.path.join(out, "overlays", f"{pid}.jpg"),
                            cv2.cvtColor(overlay(crop, A["fruits"], ok_ms), cv2.COLOR_RGB2BGR),
                            [cv2.IMWRITE_JPEG_QUALITY, 88])
                rec["status"] = "ok"
            except Exception as e:  # one failing photo must not stop the batch; the error is recorded
                rec["status"] = f"error: {type(e).__name__}: {str(e)[:150]}"
            rec["sec"] = round(time.time() - t, 2)
            jf.write(json.dumps(plain(rec), ensure_ascii=False) + "\n")
            jf.flush()
            if i % log_every == 0 or i == len(todo):
                print(f"{i}/{len(todo)} photos, {(time.time() - t0) / i:.1f} s per photo", flush=True)
            if max_hours and time.time() - t0 > max_hours * 3600:
                print(f"time limit reached after {i} photos", flush=True)
                break
    return collect(out)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--list", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--heads", default=None)
    ap.add_argument("--one-stage", action="store_true")
    ap.add_argument("--max-side", type=int, default=2048)
    ap.add_argument("--limit", type=int)
    ap.add_argument("--max-hours", type=float)
    ap.add_argument("--embeddings", action="store_true", help="save the fruit feature sets of every photo")
    ap.add_argument("--threads", type=int, default=None, help="CPU threads for PyTorch")
    a = ap.parse_args(argv)
    if a.threads:
        import torch
        torch.set_num_threads(a.threads)
    t = pd.read_csv(a.list, dtype={"photo_id": str})
    if a.limit:
        t = t.head(a.limit)
    run(t, a.out, a.heads, not a.one_stage, a.max_side, max_hours=a.max_hours, embeddings=a.embeddings)


if __name__ == "__main__":
    main()
