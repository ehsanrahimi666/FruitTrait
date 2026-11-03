# FruitTrait

[![R-CMD-check](https://github.com/ehsanrahimi666/FruitTrait/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ehsanrahimi666/FruitTrait/actions/workflows/R-CMD-check.yaml)

**Colour of fleshy fruits from field photographs**

`FruitTrait` measures the colour of single fleshy fruits of trees, shrubs and
woody climbers in field photographs. It finds the fruits, outlines each one,
rejects outlines that are too small, cut, overlapping or clustered, decides with
trained classifiers whether each outline is a single fruit with a correct
outline and whether the fruit is ripe, and returns the CIELAB colour of every
fruit and of the ripe fruits of each photo, with white-balanced colour, colour
names, the contrast with the surrounding foliage and a catalogue of how reliable
each trait is. Photos can come from your own folders or from fruiting
observations on GBIF and iNaturalist.

**Fleshy fruits of woody plants.** The fruit classifiers were trained, and the
workflow validated, on 479 iNaturalist photographs of 240 fleshy-fruited woody
species (70 families) from all continents except Antarctica, with every outline
rated by eye, on 1,699 plants photographed twice, and against the reflectance
spectra of 104 species.
`fr_in_scope()` tells whether a species belongs to this group (`fr_scope`), and
`fr_taxonomy()` warns about names that are not flowering plants.

## Installation

```r
install.packages("remotes")
remotes::install_github("ehsanrahimi666/FruitTrait")
```

Most functions run in R alone. Detection, outlining and the fruit classifiers
need the optional Python back end, installed once:

```r
library(FruitTrait)
fr_install()                 # or fr_install(cpu_only = TRUE) on a laptop
fr_use_backend()             # at the start of each R session
fr_backend_available()       # TRUE when ready
```

The first run downloads the model weights (about 1.5 GB). The back end runs in
a separate Python process, so PyTorch is never loaded into R. On two CPU cores
a photo takes about 35 s.

## Where the photos come from

```r
# one photo
res <- fr_process("PATH_TO_ONE_PHOTO.jpg", out_dir = "PATH_TO_OUTPUT_FOLDER")

# a folder of photos (searched recursively)
res <- fr_process("PATH_TO_PHOTO_FOLDER", out_dir = "PATH_TO_OUTPUT_FOLDER")

# a folder with one sub-folder per species, e.g. photos/Sorbus_aucuparia/*.jpg
ph  <- fr_read_folder("PATH_TO_SPECIES_FOLDERS")   # species from folder names
ph  <- fr_taxonomy(ph)                             # adds family, order and scope (GBIF)
res <- fr_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")

# GBIF or iNaturalist: fruiting observations, CC0 and CC BY photos, original size
ph  <- fr_gbif_photos("Sorbus aucuparia", n = 100, country = "DE")
ph  <- fr_inat_photos("Sorbus aucuparia", n = 100)
ph  <- fr_download_photos(ph, dir = "PATH_TO_PHOTO_FOLDER")
res <- fr_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")
writeLines(fr_attribution(ph), "attribution.txt")
```

## Quick start (runs as-is, no Python needed)

```r
library(FruitTrait)
ex <- fr_example_photos()                          # CC0 / CC BY photos
fr_plot_fruits(ex$crop[1], ex$labels[1], main = ex$species[1])
fr_fruit_colour(ex$crop[1], ex$labels[1])          # colour of every outlined fruit, in R
res <- fr_example_results()                        # back-end results, measurable fruits
summary(res)
fr_fruits(res, "measurable")
fr_contact_sheet(res, ncol = 4)
fr_trait_info(verdict = "reliable")
```

`system.file("scripts", "quickstart.R", package = "FruitTrait")` runs every
step above and more; `vignette("FruitTrait")` is a worked example.

## Output

`fr_process()` writes `traits.csv` (one row per photo), `fruits.csv` (one row
per detected fruit, with its outline checks, its outline polygon in the
original photo, its CIELAB colour, white-balanced colour and colour name, and
the classifier probabilities `p_valid`, `p_ripe` and `p_dry`), crops, fruit
label maps and overlays. A fruit is measurable when it passed the outline
checks and the classifiers rate it a single fruit with a correct outline
(`p_valid >= 0.5`) and ripe (`p_ripe >= 0.5`); the colour of a photo (`m_L`,
`m_a`, `m_b`, `m_C`, `m_hue`, `m_name`) is the mean colour of its measurable
fruits, and the `v_` traits give the colour of all valid fruits, ripe or not.
`fr_filter()` applies the rules `"measurable"`, `"valid"`, `"outlined"` or
`"none"`.

## Validation

On the 479 benchmark photographs, 1,367 of 2,060 outlines passed the outline
checks, and 63.4% of these were single fleshy fruits with a correct outline.
With folds grouped by species, the valid classifier reached a cross-validated
AUC of 0.92 and the ripe classifier 0.92; at the default threshold of 0.5,
92.4% of the outlines kept were single fruits and 81.2% of the fruits
kept as ripe had been rated ripe (`fr_benchmark$rules`). The dry classifier
(AUC 0.61) is not used. Between two photos of the same plant, the intraclass
correlation of the photo colour was 0.76 for *L*\*, 0.90 for *a*\*, 0.88 for
*b*\*, 0.88 for chroma and 0.82 for hue (633 plants), and simulated camera
changes and reduction of the fruits to 8 pixels across left the colour of each
fruit nearly unchanged (ICC at least 0.936). Across 104 species, chroma in the
photographs was correlated with the saturation of reflectance spectra (Spearman
ρ = 0.62). R and the back end give the same fruit colour (median CIEDE2000
difference 0.09). `fr_catalogue` gives the definition, repeatability,
invariance and verdict of all 32 traits.

## Functions

| Task | Functions |
|---|---|
| Photos | `fr_read_folder()`, `fr_taxonomy()`, `fr_in_scope()`, `fr_gbif_photos()`, `fr_inat_photos()`, `fr_download_photos()`, `fr_attribution()`, `fr_example_photos()` |
| Back end | `fr_install()`, `fr_use_backend()`, `fr_backend_available()`, `fr_process()`, `fr_example_results()` |
| Results | `fr_read_results()`, `fr_fruits()`, `fr_filter()`, `summary()`, `plot()`, `fr_summarise()`, `fr_read_dataset()` |
| Colour in R | `fr_read_image()`, `fr_read_labels()`, `fr_read_mask()`, `fr_write_mask()`, `fr_rgb_to_lab()`, `fr_fruit_colour()`, `fr_colour_name()`, `fr_delta_e()`, `fr_spectra_to_lab()` |
| Colour checks and augmentation | `fr_colour_check()`, `fr_white_balance()`, `fr_augment()`, `fr_augment_check()` |
| Reliability | `fr_trait_info()`, `fr_reliable_traits()`, `fr_icc()`, `fr_repeatability()`, `fr_invariance()` |
| Plots | `fr_plot_fruits()`, `fr_plot_colours()`, `fr_contact_sheet()`, `fr_plot_repeatability()` |
| Data | `fr_scope`, `fr_catalogue`, `fr_validation`, `fr_benchmark`, `fr_reference` |

## Limitations

- Colour is device colour in sRGB under unknown light: no ultraviolet and no
  frugivore vision. Photographs preserve differences between species seen in
  reflectance spectra (`fr_reference`), but use spectra (e.g. with pavo) for
  visual modelling.
- The ripe classifier keeps ripe red, black and blue fruits more often than
  ripe yellow, orange and brown fruits (`fr_benchmark$ripe_colour`); repeat
  analyses of colour composition with the `v_` traits or other thresholds.
- Fruits must be outlined one by one and be at least 800 pixels (about 32
  pixels across); clusters of small fruits are left out.
- The outline ratings used to train and evaluate the classifiers were not
  compared with independent ratings.
- Repeatability comes from two photos of the same observation; variation across
  days, observers and cameras will be larger.
- Absolute fruit size needs a scale in the photo; `m_diam_px` is in pixels.
- SAM 2.1 is loaded through ultralytics, which is licensed under AGPL-3.0.

## Citation

See `citation("FruitTrait")`.

## License

GPL (>= 3). The example and gallery photos keep their own licences (CC0 1.0 or
CC BY 4.0); see `fr_example_photos()`, `inst/extdata/ATTRIBUTION.csv` and
`inst/extdata/gallery/ATTRIBUTION.csv`.
