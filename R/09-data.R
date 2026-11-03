#' Species in the scope of the method
#'
#' Fleshy-fruited woody species (trees, shrubs and woody climbers) for which the
#' FruitTrait photo pipeline was built: accepted species of the World Checklist
#' of Vascular Plants that are woody and have fleshy fruits.
#'
#' Woody: the dominant category of the global woodiness database (Zanne et al.
#' 2014); species not in it, or of variable woodiness, are woody when their life
#' form in the World Checklist contains tree, shrub, subshrub or liana and does
#' not begin with annual, biennial or perennial. Fleshy: species listed with
#' fleshy, animal-dispersed fruits by Sinnott-Armstrong et al. (2018, 2021) or in
#' PalmTraits 1.0 (Kissling et al. 2019) (`fleshy_source = "species"`); other
#' species of a genus with at least five listed species, or with at least 20% of
#' its species listed, are included at genus level (`fleshy_source = "genus"`).
#' Names of the sources were resolved to accepted names of the World Checklist.
#'
#' @format A data frame with one row per species:
#' \describe{
#'   \item{species}{accepted name (World Checklist of Vascular Plants)}
#'   \item{family}{family}
#'   \item{woody_source}{`"woodiness database"` or `"WCVP life form"`}
#'   \item{fleshy_source}{`"species"` (listed) or `"genus"` (genus-level rule)}
#' }
#' @source World Checklist of Vascular Plants (Govaerts et al. 2021,
#'   \doi{10.1038/s41597-021-00997-6}); Zanne et al. (2014,
#'   \doi{10.1038/nature12872}; data \doi{10.5061/dryad.63q27});
#'   Sinnott-Armstrong et al. (2018, \doi{10.1111/geb.12801}; 2021,
#'   \doi{10.1111/ele.13753}); Kissling et al. (2019,
#'   \doi{10.1038/s41597-019-0189-0}).
#' @seealso [fr_in_scope()]
#' @examples
#' head(fr_scope)
#' table(fr_scope$fleshy_source)
"fr_scope"

#' Repeatability pairs: two photographs of the same plant
#'
#' Traits of two photographs of the same plant taken on the same occasion
#' (research-grade 'iNaturalist' observations with at least two photographs),
#' processed independently by the back end: 3,398 photographs of 1,699
#' observations of 130 species in 50 families. Used to estimate the
#' photo-to-photo repeatability of the traits in [fr_catalogue] (see
#' [fr_repeatability()]).
#'
#' @format A data frame with one row per photograph:
#' \describe{
#'   \item{occurrenceKey}{GBIF occurrence key of the observation (character)}
#'   \item{photo_id}{iNaturalist photo identifier (character)}
#'   \item{position}{`"first"` or `"second"` photograph of the observation}
#'   \item{species, family}{GBIF backbone species and family}
#'   \item{license}{licence of the photograph}
#'   \item{latitude, longitude, year}{coordinates (WGS84) and year of the observation}
#'   \item{both_measurable}{both photographs of the observation are measurable}
#'   \item{measurable}{the photograph has at least one measurable fruit}
#'   \item{n_fruits_ok, m_n, m_L, m_a, m_b, m_C, m_hue, m_name, m_wb_L, m_wb_a,
#'     m_wb_b, m_diam_px, v_n, v_L, v_a, v_b, v_C, v_hue, v_name, ring_dE00,
#'     ring_dL, gloss_frac, fruit_elongation, sharp}{photo traits; see
#'     [fr_trait_info()]}
#' }
#' @source Research-grade 'iNaturalist' observations served by GBIF; traits
#'   computed with the back end of this package.
#' @seealso [fr_repeatability()], [fr_catalogue]
#' @examples
#' v <- fr_validation[fr_validation$both_measurable, ]
#' fr_repeatability(v, traits = c("m_L", "m_a", "m_b", "m_C"), boot = 0)
"fr_validation"

#' Photo colour and reflectance spectra of reference species
#'
#' Mean colour of the measurable photographs of 104 species whose fruit
#' reflectance spectra were compiled by Sinnott-Armstrong et al. (2018), with
#' the spectral measures of that compilation. The photographs are
#' research-grade 'iNaturalist' photographs of fleshy-fruited woody plants
#' processed with the back end of this package.
#'
#' @format A data frame with one row per species:
#' \describe{
#'   \item{species}{species name}
#'   \item{photos}{number of measurable photographs}
#'   \item{L, a, b, C, hue_deg}{mean CIELAB colour of the measurable photographs}
#'   \item{photo_name}{most frequent colour name ([fr_colour_name()]) among the
#'     measurable photographs}
#'   \item{spectrum_colour}{colour description of the first spectrum entry of the
#'     species in the compilation}
#'   \item{n_entries}{number of spectrum entries}
#'   \item{spectral_lightness, spectral_saturation, spectral_hue_nm}{mean
#'     spectral lightness, saturation and hue (nm) of the entries}
#' }
#' @source Sinnott-Armstrong et al. (2018, \doi{10.1111/geb.12801}).
#' @seealso [fr_spectra_to_lab()]
#' @examples
#' with(fr_reference, cor(C, spectral_saturation, method = "spearman"))
"fr_reference"

#' Rated benchmark fruits and validation of the photo pipeline
#'
#' Outlines produced by the back end on 479 benchmark photographs of 240
#' fleshy-fruited woody species in 70 families, each outline that passed the
#' outline checks rated by eye, and the results of the validation analyses.
#'
#' @format A list:
#' \describe{
#'   \item{photos}{one row per benchmark photograph: coordinates and year of
#'     the observation, detections, fruits
#'     outlined and the number that failed each outline check (`n_small`,
#'     `n_fill`, `n_shape`, `n_border`, `n_overlap`, `n_size`), passed
#'     (`n_passed`) and were rated (`n_rated`)}
#'   \item{outlines}{one row per rated outline: class (`cls`: `F` single fleshy
#'     fruit, `Y` single dry fruit, `P` partial or wrong outline, `C` several
#'     fruits, `N` not a fruit), stage of single fruits (`stage`: `R` ripe, `U`
#'     unripe, `D` dried or damaged, empty when undecided), size, colour and the
#'     out-of-fold probabilities of the valid, ripe and dry classifiers}
#'   \item{heads}{cross-validated and nested cross-validated performance of
#'     the three classifiers and whether each met the criteria for use (AUC at
#'     least 0.80 and balanced accuracy at least 0.75)}
#'   \item{feature_sets}{performance of the candidate feature sets}
#'   \item{rules}{share of outlines kept, precision and recall of the valid
#'     and ripe classifiers at several thresholds (out-of-fold probabilities)}
#'   \item{ripe_colour}{share of rated ripe and unripe fruits classified ripe,
#'     by colour name}
#'   \item{invariance}{ICC between the original crop and each simulated camera
#'     change and resolution (see [fr_invariance()]), fruits rated single ripe
#'     fleshy fruits}
#'   \item{invariance_n}{fruits and crops compared for each change}
#'   \item{augmentation}{change of the photo colour under training-style
#'     augmentations (see [fr_augment_check()])}
#'   \item{r_python}{colour of each outlined fruit computed in R
#'     ([fr_fruit_colour()]) and by the back end on the same crops}
#' }
#' @source Research-grade 'iNaturalist' photographs served by GBIF.
#' @seealso [fr_catalogue], [fr_filter()]
#' @examples
#' fr_benchmark$heads[, c("head", "cv_auc", "oof_balanced_accuracy", "usable")]
#' fr_benchmark$rules
"fr_benchmark"

#' Trait catalogue: definitions and reliability
#'
#' Definition, level, unit and reliability of every trait returned by the
#' package. Numeric traits are rated `"reliable"` when their photo-to-photo
#' repeatability ([fr_repeatability()] on pairs measurable in both photos) is
#' at least 0.6, their smallest invariance to the photometric changes of
#' [fr_invariance()] (white balance, exposure, JPEG compression, blur) at
#' least 0.8 and their ICC at a median fruit diameter of 32 pixels at least
#' 0.8; `"use with care"` when the repeatability is at least 0.4 and the
#' photometric invariance, if tested, at least 0.7; and `"not reliable"`
#' otherwise. Colour names are rated by the share of pairs with the same name
#' (at least 0.8 reliable, at least 0.6 use with care), and classifier
#' probabilities by the criteria for use of [fr_benchmark]. Fruit-level colour
#' traits take the repeatability of the photo mean of the measurable fruits.
#'
#' @format A data frame with one row per trait:
#' \describe{
#'   \item{trait, level, family, type, unit}{name, level (`"photo"` or
#'     `"fruit"`), trait family, type and unit}
#'   \item{verdict, reason}{verdict and the values that decided it}
#'   \item{icc_measurable, icc_lower, icc_upper}{repeatability between photos
#'     measurable in both, with a bootstrap 95% interval}
#'   \item{icc_all}{repeatability over all pairs with values in both photos}
#'   \item{n_pairs}{pairs used}
#'   \item{agreement}{share of pairs with the same colour name}
#'   \item{inv_photometric_min, inv_geometric_min}{smallest ICC under the
#'     photometric and geometric (rotation, perspective) changes}
#'   \item{icc_D32}{ICC at a median fruit diameter of 32 pixels}
#'   \item{min_D_px}{smallest fruit diameter (pixels) with an ICC of at least 0.8}
#'   \item{computed_by}{whether R ([fr_fruit_colour()]) also computes the trait}
#'   \item{description}{definition}
#' }
#' @seealso [fr_trait_info()], [fr_reliable_traits()]
#' @examples
#' fr_catalogue[, c("trait", "verdict", "icc_measurable", "inv_photometric_min")]
"fr_catalogue"
