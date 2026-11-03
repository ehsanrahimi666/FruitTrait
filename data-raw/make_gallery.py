"""Gallery of rated benchmark outlines shipped with the FruitTrait R package (inst/extdata/gallery).

python make_gallery.py <package dir> <project dir> <benchmark run dir> [<benchmark run dir> ...]

Two outlines per rating class (single fleshy fruit ripe, unripe and damaged; single dry fruit; partial or wrong
outline; several fruits; not a fruit) are drawn at random (seed 1) among outlines of CC0 or CC BY photographs that
are at least 50 px across, from different photographs. For each, the crop region around the fruit (its box enlarged
by one box size on each side, as in the rating tiles) and the outline are written, with the rights holder and
licence of the photograph from the iNaturalist API.
"""
import json
import os
import sys
import time
import urllib.request

import cv2
import numpy as np
import pandas as pd

pkg, project, runs = sys.argv[1], sys.argv[2], sys.argv[3:]
out = os.path.join(pkg, "inst", "extdata", "gallery")
os.makedirs(out, exist_ok=True)
ol = pd.read_csv(os.path.join(pkg, "data-raw", "validation", "benchmark_outlines.csv"), dtype={"photo_id": str})
ph = pd.read_csv(os.path.join(project, "03_validation", "benchmark", "benchmark_photos.csv"),
                 dtype={"photo_id": str, "inat_obs_id": str})
ol["key"] = ol.cls + ol.stage.fillna("")
ol.loc[ol.cls == "Y", "key"] = "Y"
classes = [("FR", "Single fleshy fruit, ripe"), ("FU", "Single fleshy fruit, unripe"),
           ("FD", "Single fleshy fruit, damaged"), ("Y", "Single dry fruit"),
           ("P", "Partial or wrong outline"), ("C", "Several fruits"), ("N", "Not a fruit")]
cand = ol[ol.license.isin(["cc0", "cc-by"]) & (ol.diam_px >= 50) & ol.label.notna()]
rng = np.random.default_rng(1)
rows, used = [], set()
for key, title in classes:
    c = cand[(cand.key == key) & ~cand.photo_id.isin(used)]
    c = c.iloc[rng.permutation(len(c))]
    c = c.drop_duplicates("photo_id").head(2)
    if len(c) < 2:
        raise SystemExit(f"fewer than two outlines for class {key}")
    used.update(c.photo_id)
    for _, r in c.iterrows():
        rows.append(dict(r, class_title=title))


def run_file(pid, suffix):
    for r in runs:
        f = os.path.join(r, "crops", f"{pid}{suffix}")
        if os.path.exists(f):
            return f
    raise SystemExit(f"crop not found: {pid}{suffix}")


def inat_photo(obs_id, photo_id):
    url = f"https://api.inaturalist.org/v1/observations/{obs_id}"
    req = urllib.request.Request(url, headers={"User-Agent": "FruitTrait R package (data preparation)"})
    for attempt in range(4):
        try:
            with urllib.request.urlopen(req, timeout=30) as resp:
                res = json.load(resp)["results"][0]
            break
        except Exception:
            time.sleep(2 ** attempt)
    else:
        raise SystemExit(f"iNaturalist API not reachable for observation {obs_id}")
    time.sleep(1)
    for p in res.get("photos", []):
        if str(p["id"]) == photo_id:
            user = res["user"]
            return dict(rights_holder=user.get("name") or user.get("login"), license_api=p.get("license_code"),
                        attribution=p.get("attribution"))
    raise SystemExit(f"photo {photo_id} not found in observation {obs_id}")


tab = []
for i, r in enumerate(rows, 1):
    crop = cv2.imread(run_file(r["photo_id"], ".jpg"))
    lab = cv2.imread(run_file(r["photo_id"], "_fruits.png"), cv2.IMREAD_UNCHANGED)
    if lab.ndim == 3:
        lab = lab[..., 0]
    m = lab == int(r["label"])
    ys, xs = np.where(m)
    x0, x1, y0, y1 = xs.min(), xs.max() + 1, ys.min(), ys.max() + 1
    bw, bh = x1 - x0, y1 - y0
    X0, X1 = max(0, x0 - bw), min(crop.shape[1], x1 + bw)
    Y0, Y1 = max(0, y0 - bh), min(crop.shape[0], y1 + bh)
    tile = crop[Y0:Y1, X0:X1]
    s = 400 / max(tile.shape[:2])
    tm = m[Y0:Y1, X0:X1].astype(np.uint8)
    if s < 1:
        tile = cv2.resize(tile, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
        tm = cv2.resize(tm, (tile.shape[1], tile.shape[0]), interpolation=cv2.INTER_NEAREST)
    name = f"{i:02d}_{r['photo_id']}_{int(r['fruit'])}"
    cv2.imwrite(os.path.join(out, name + ".jpg"), tile, [cv2.IMWRITE_JPEG_QUALITY, 90])
    cv2.imwrite(os.path.join(out, name + ".png"), tm)
    p = ph[ph.photo_id == r["photo_id"]].iloc[0]
    a = inat_photo(p.inat_obs_id, r["photo_id"])
    if a["license_api"] is None or a["license_api"].lower() not in ("cc0", "cc-by"):
        raise SystemExit(f"licence of photo {r['photo_id']} is no longer CC0 or CC BY: {a['license_api']}")
    tab.append(dict(file=name, photo_id=r["photo_id"], fruit=int(r["fruit"]), species=r["species"],
                    family=r["family"], cls=r["cls"], stage=r["stage"], class_title=r["class_title"],
                    diam_px=round(r["diam_px"], 1), p_valid=round(r["p_valid"], 3),
                    p_ripe=None if pd.isna(r["p_ripe"]) else round(r["p_ripe"], 3),
                    rights_holder=a["rights_holder"], license=a["license_api"].lower(),
                    attribution=a["attribution"],
                    observation_url=f"https://www.inaturalist.org/observations/{p.inat_obs_id}"))
pd.DataFrame(tab).to_csv(os.path.join(out, "ATTRIBUTION.csv"), index=False)
print(f"{len(tab)} gallery tiles written to {out}")
