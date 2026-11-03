"""Validation tables of the FruitTrait package (data-raw/validation) from the benchmark and validation runs.

python make_validation_files.py <project dir> <benchmark run dir> [<benchmark run dir> ...] --out <dir>

project dir: holds 03_validation/benchmark (rated benchmark fruits, photos, out-of-fold predictions of the fruit
heads trained on CPU features) and 05_results/analysis (assembled photos and validation photos, colour of the
reference species). benchmark run dir: output folder of fruittrait.run_photos for the benchmark photos (CPU), with
part_*.csv (one row per photo) and fruits_*.csv (one row per detected fruit).
Writes benchmark_outlines.csv, benchmark_photos.csv, validation_pairs.csv and reference_species.csv.
"""
import argparse
import glob
import os

import numpy as np
import pandas as pd

ap = argparse.ArgumentParser()
ap.add_argument("project")
ap.add_argument("runs", nargs="+")
ap.add_argument("--out", required=True)
a = ap.parse_args()
os.makedirs(a.out, exist_ok=True)
B = os.path.join(a.project, "03_validation", "benchmark")
A = os.path.join(a.project, "05_results", "analysis")

# --- benchmark: rated outlines with out-of-fold head probabilities and back-end colour
lab = pd.read_csv(os.path.join(B, "benchmark_labels.csv"), dtype={"photo_id": str})
ph = pd.read_csv(os.path.join(B, "benchmark_photos.csv"), dtype={"photo_id": str})
oof = pd.read_csv(os.path.join(B, "fruit_heads_cpu", "fruit_heads_oof.csv"), dtype={"photo_id": str})
oof = oof.pivot_table(index=["photo_id", "fruit"], columns="head", values="p_oof").reset_index()
oof.columns = ["photo_id", "fruit"] + [f"p_{c}" for c in oof.columns[2:]]
fr = pd.concat([pd.read_csv(f, dtype={"photo_id": str}) for r in a.runs
                for f in sorted(glob.glob(os.path.join(r, "fruits_*.csv")))], ignore_index=True)
fr = fr.drop_duplicates(["photo_id", "fruit"], keep="last")
col = ["photo_id", "fruit", "label", "ok", "fail", "px", "diam_px_photo", "elongation", "L", "a", "b", "C",
       "hue_deg", "wb_L", "wb_a", "wb_b"]
out = lab[["photo_id", "fruit", "species", "cls", "stage", "name"]].merge(
    ph[["photo_id", "family", "photo_license"]], on="photo_id", how="left").merge(
    fr[col], on=["photo_id", "fruit"], how="left").merge(oof, on=["photo_id", "fruit"], how="left")
out = out.rename(columns={"photo_license": "license", "diam_px_photo": "diam_px"}).drop(columns=["ok", "fail"])
out.to_csv(os.path.join(a.out, "benchmark_outlines.csv"), index=False)

parts = pd.concat([pd.read_csv(f, dtype={"photo_id": str}) for r in a.runs
                   for f in sorted(glob.glob(os.path.join(r, "part_*.csv")))], ignore_index=True)
parts = parts.drop_duplicates("photo_id", keep="last")
reasons = ["small", "fill", "shape", "border", "overlap", "size"]
f = fr.assign(fail=fr.fail.fillna(""))
fails = pd.DataFrame({"n_outlined": f.groupby("photo_id").size(),
                      "n_passed": f.groupby("photo_id").ok.apply(lambda s: int(s.astype(str).str.lower().eq("true").sum())),
                      **{f"n_{r}": f.groupby("photo_id").fail.apply(lambda s, r=r: int(s.str.split(";").apply(
                          lambda x: r in x).sum())) for r in reasons}})
keep = ["photo_id", "n_det1", "n_det", "n_cluster_boxes", "n_fruit_boxes", "n_fruits_ok", "cut_by_frame",
        "display_frac"]
ph["year"] = pd.to_datetime(ph.observed_on, errors="coerce").dt.year.astype("Int64")
bp = ph[["photo_id", "species", "family", "photo_license", "inat_obs_id", "latitude", "longitude", "year"]].merge(
    parts[[c for c in keep if c in parts]], on="photo_id", how="left").merge(
    fails.reset_index(), on="photo_id", how="left")
for c in ["n_outlined", "n_passed"] + [f"n_{r}" for r in reasons]:
    bp[c] = bp[c].fillna(0).astype(int)
bp["n_rated"] = bp.photo_id.map(lab.groupby("photo_id").size()).fillna(0).astype(int)
for f in ("fruit_heads_metrics.json", "fruit_heads_eval.csv"):
    with open(os.path.join(B, "fruit_heads_cpu", f), "rb") as src, open(os.path.join(a.out, f), "wb") as dst:
        dst.write(src.read())
bp.rename(columns={"photo_license": "license"}).to_csv(os.path.join(a.out, "benchmark_photos.csv"), index=False)

# --- repeatability: first photo of each observation (design) and its second photo (validation set "pair")
T = ["measurable", "n_fruits_ok", "m_n", "m_L", "m_a", "m_b", "m_C", "m_hue", "m_name", "m_wb_L", "m_wb_a",
     "m_wb_b", "m_diam_px", "v_n", "v_L", "v_a", "v_b", "v_C", "v_hue", "v_name", "ring_dE00", "ring_dL",
     "gloss_frac", "fruit_elongation", "sharp"]
meta = ["photo_id", "gbifID", "species", "family", "photo_license"]
where = ["latitude", "longitude", "eventDate"]
des = pd.read_parquet(os.path.join(A, "assembled", "photos.parquet"), columns=meta + where + T)
val = pd.read_parquet(os.path.join(A, "assembled", "validation.parquet"), columns=meta + T + ["set"])
val = val[val["set"] == "pair"].drop(columns="set")
ids = lambda s: s.astype("string").str.replace(r"\.0$", "", regex=True)
des["gbifID"], val["gbifID"] = ids(des.gbifID), ids(val.gbifID)
des = des.dropna(subset=["gbifID"]).drop_duplicates("gbifID")
val = val[val.gbifID.isin(set(des.gbifID))]
first = des[des.gbifID.isin(set(val.gbifID))].assign(position="first")
second = val.assign(position="second")
second = second.merge(first[["gbifID"] + where], on="gbifID", how="left")
pairs = pd.concat([first, second], ignore_index=True).rename(columns={"gbifID": "occurrenceKey",
                                                                      "photo_license": "license"})
pairs["year"] = pd.to_datetime(pairs.eventDate.astype(str).str[:10], errors="coerce").dt.year.astype("Int64")
tf = lambda s: s.astype(str).str.lower().isin(["true", "1"])
pairs["measurable"] = tf(pairs.measurable)
both = pairs.groupby("occurrenceKey").measurable.transform("sum") == 2
pairs["both_measurable"] = both
pairs = pairs.sort_values(["occurrenceKey", "position"])
pairs[["occurrenceKey", "photo_id", "position", "species", "family", "license", "latitude", "longitude", "year",
       "both_measurable"] + T].to_csv(
    os.path.join(a.out, "validation_pairs.csv"), index=False)

# --- colour of the reference species in photos and in reflectance spectra (Sinnott-Armstrong et al. 2018)
ref = pd.read_csv(os.path.join(A, "technical_validation", "tv_spectra_species.csv"))
ref = ref.rename(columns={"key": "species", "name": "photo_name", "colour": "spectrum_colour",
                          "Lightness": "spectral_lightness", "Saturation": "spectral_saturation",
                          "Hue": "spectral_hue_nm"})
ref["hue_deg"] = np.degrees(np.arctan2(ref.b, ref.a)) % 360
ref[["species", "photos", "L", "a", "b", "C", "hue_deg", "photo_name", "spectrum_colour", "n_entries",
     "spectral_lightness", "spectral_saturation", "spectral_hue_nm"]].to_csv(
    os.path.join(a.out, "reference_species.csv"), index=False)
print({f: len(pd.read_csv(os.path.join(a.out, f))) for f in sorted(os.listdir(a.out)) if f.endswith(".csv")})
