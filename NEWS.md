# FruitTrait 0.1.0

* First release: two-stage fruit detection with 'Grounding DINO', outlines of single fruits with 'SAM' 2.1, outline
  checks, fruit classifiers (single fruit with a correct outline, ripe, dry), CIELAB colour of every fruit and of the
  ripe fruits of each photo, colour of the surroundings, white-balanced colour.
* R functions to find and download fruiting observations on iNaturalist and GBIF, match names to the GBIF backbone
  (`fr_taxonomy()`), read, filter and summarise results, read the tables of the published data set
  (`fr_read_dataset()`), recompute fruit colour from crops and label maps, convert reflectance spectra, check colour
  casts, plot outlines and colours, and look up the scope of the method (`fr_scope`).
* Trait reliability: trait catalogue with repeatability, invariance and verdicts (`fr_catalogue`, `fr_trait_info()`,
  `fr_reliable_traits()`), intraclass correlation and repeatability with bootstrap intervals (`fr_icc()`,
  `fr_repeatability()`, `fr_plot_repeatability()`), simulated camera changes and augmentations (`fr_augment()`,
  `fr_invariance()`, `fr_augment_check()`), and the validation data (`fr_validation`, `fr_benchmark`,
  `fr_reference`).
