# Data preparation

Scripts that build the data sets and example files of the package. They are not part of the installed package.

| Script | Output |
|---|---|
| `make_sysdata.R` | `R/sysdata.rda` (CIE 1931 colour-matching functions and illuminant D65, from `cie1931_d65.csv`) |
| `make_scope.R` | `data/fr_scope.rda` (fleshy-fruited woody species) |
| `make_examples.py` | `inst/extdata` (example photographs, crops, fruit label maps and back-end results) |
| `make_gallery.py` | `inst/extdata/gallery` (rated benchmark outlines, two per rating class) |
| `validation/code/make_validation_files.py` | `validation/*.csv` (benchmark outlines and photographs, repeatability pairs, reference species, classifier performance) |
| `validation/code/invariance_agreement.R` | `validation/invariance_*.csv`, `validation/augmentation.csv`, `validation/r_python.csv` |
| `make_datasets.R` | `data/fr_validation.rda`, `data/fr_reference.rda`, `data/fr_benchmark.rda`, `data/fr_catalogue.rda` |

Order: `make_validation_files.py`, `make_datasets.R` (writes `fr_validation`, which the invariance script uses),
`invariance_agreement.R`, then `make_datasets.R` again. Each script lists its arguments in its header.

## Validation tables

- `benchmark_photos.csv`: benchmark photographs (iNaturalist photo and observation identifiers, species, family,
  licence, coordinates, year) with the number of detections, of outlines and of outlines that failed each check.
- `benchmark_outlines.csv`: every outline that passed the checks, with its rating (`cls`: F single fleshy fruit, Y single
  dry fruit, P partial or wrong outline, C several fruits, N not a fruit; `stage` of single fruits: R ripe, U unripe,
  D dried or damaged), size, colour and the out-of-fold probabilities of the valid, ripe and dry classifiers.
- `validation_pairs.csv`: first and second photograph of observations photographed twice (GBIF occurrence key,
  iNaturalist photo identifier, licence, coordinates, year) with their photograph traits.
- `reference_species.csv`: mean photograph colour of species with published fruit reflectance spectra and the spectral
  measures of Sinnott-Armstrong et al. (2018, Global Ecology and Biogeography 27: 1339-1351).
- `fruit_heads_metrics.json`, `fruit_heads_eval.csv`: cross-validated performance of the classifiers shipped with the
  back end and of all candidate feature sets.
