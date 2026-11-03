"""Example data of the FruitTrait R package (inst/extdata) from CC0 or CC BY iNaturalist photos.

python make_examples.py <attribution csv> <package dir> [--heads heads_fruitdata.json] [--weights-dir <dir>]
attribution csv: photo_id, photo_url, species, family, latitude, longitude, observation_url, rights_holder, license,
attribution. The photos are downloaded, processed with the back end of the package (fruittrait.run_photos, the
same code as fr_process()), reduced to 1024 px on the longer side and written with their crops, fruit label maps,
traits.csv, fruits.csv, input.csv and ATTRIBUTION.csv.
"""
import argparse
import os
import shutil
import subprocess
import sys
import tempfile

import cv2
import pandas as pd

ap = argparse.ArgumentParser()
ap.add_argument("attribution")
ap.add_argument("pkg")
ap.add_argument("--heads", default=None)
ap.add_argument("--weights-dir", default=os.path.dirname(os.path.abspath(__file__)),
                help="folder holding (or receiving) the SAM 2.1 weights sam2.1_b.pt; the back end runs there")
a = ap.parse_args()
att = pd.read_csv(a.attribution, dtype={"photo_id": str})
if not att.license.str.lower().isin(["cc0", "cc-by"]).all():
    raise SystemExit("only CC0 and CC BY photos can be shipped")
ext = os.path.join(a.pkg, "inst", "extdata")
for d in ("photos", "crops", "fruits"):
    shutil.rmtree(os.path.join(ext, d), ignore_errors=True)
    os.makedirs(os.path.join(ext, d))
work = tempfile.mkdtemp()
lst = os.path.join(work, "input.csv")
att[["photo_id", "photo_url", "species", "family", "latitude", "longitude"]].to_csv(lst, index=False)
env = dict(os.environ, PYTHONPATH=os.path.join(a.pkg, "inst", "python"))
cmd = [sys.executable, "-m", "fruittrait.run_photos", "--list", lst, "--out", work] + (["--heads", a.heads] if a.heads else [])
subprocess.run(cmd, env=env, check=True, cwd=a.weights_dir)
for pid in att.photo_id:
    img = cv2.imread(os.path.join(work, "photos", f"{pid}.jpg"))
    s = 1024 / max(img.shape[:2])
    if s < 1:
        img = cv2.resize(img, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
    cv2.imwrite(os.path.join(ext, "photos", f"{pid}.jpg"), img, [cv2.IMWRITE_JPEG_QUALITY, 88])
    shutil.copy2(os.path.join(work, "crops", f"{pid}.jpg"), os.path.join(ext, "crops", f"{pid}.jpg"))
    shutil.copy2(os.path.join(work, "fruits", f"{pid}.png"), os.path.join(ext, "fruits", f"{pid}.png"))
t = pd.read_csv(os.path.join(work, "traits.csv"), dtype={"photo_id": str})
t = t.drop(columns=[c for c in ("path", "photo_url") if c in t])
t.to_csv(os.path.join(ext, "traits.csv"), index=False)
shutil.copy2(os.path.join(work, "fruits.csv"), os.path.join(ext, "fruits.csv"))
att[["photo_id", "species", "family"]].to_csv(os.path.join(ext, "input.csv"), index=False)
att[["photo_id", "species", "family", "latitude", "longitude", "observation_url", "rights_holder", "license",
     "attribution"]].to_csv(os.path.join(ext, "ATTRIBUTION.csv"), index=False)
print(f"{len(att)} example photos written to {ext}")
