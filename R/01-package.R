#' FruitTrait: colour of fleshy fruits from field photographs
#'
#' @description
#' \pkg{FruitTrait} measures the colour of single fleshy fruits in field
#' photographs, for example the research-grade 'iNaturalist' photos of fruiting
#' plants served by 'GBIF'. The method was built for fleshy-fruited trees,
#' shrubs and woody climbers ([fr_scope]); it measures fruits that are large
#' enough in the photo and outlined one by one, and leaves out clusters,
#' fruits cut by the frame or by other fruits, and outlines that are not a
#' single fruit.
#'
#' @section Workflow:
#' \enumerate{
#'   \item Find photos: [fr_inat_photos()], [fr_gbif_photos()] and
#'     [fr_download_photos()] (fruiting observations by default), or use your
#'     own: one photo, a folder, or a folder with one sub-folder per species
#'     ([fr_read_folder()]). [fr_in_scope()] tells whether a species is in the
#'     scope of the method.
#'   \item Process photos with the optional 'Python' back end
#'     ([fr_install()], [fr_use_backend()], [fr_process()]): two-pass
#'     'Grounding DINO' detection, 'SAM' 2.1 outline of every single fruit,
#'     outline checks, fruit heads (single fruit with a correct outline,
#'     ripe, dry) and colour.
#'   \item Read and filter results: [fr_read_results()], [fr_fruits()],
#'     [fr_filter()].
#'   \item Check colour: [fr_colour_check()], [fr_white_balance()],
#'     [fr_fruit_colour()] (the colour of outlined fruits computed in R).
#'   \item Summarise and plot: [fr_summarise()], [fr_plot_fruits()],
#'     [fr_plot_colours()], [fr_contact_sheet()].
#'   \item Choose traits by their reliability: [fr_trait_info()] and
#'     [fr_reliable_traits()] ([fr_catalogue]); repeat the tests on your own
#'     photos with [fr_repeatability()], [fr_invariance()], [fr_augment()] and
#'     [fr_augment_check()].
#' }
#' Colour names ([fr_colour_name()]) and the conversion of reflectance spectra
#' to CIELAB ([fr_spectra_to_lab()]) use the same rules as the back end. The
#' validation data are in [fr_validation], [fr_benchmark] and [fr_reference];
#' the tables of the published data set are read with [fr_read_dataset()].
#'
#' @examples
#' res <- fr_example_results()
#' summary(res)
#' head(fr_fruits(res)[, c("photo_id", "fruit", "L", "a", "b", "name")])
#' @keywords internal
"_PACKAGE"

.fr_py_packages <- c("numpy", "pandas", "opencv-python-headless", "pillow", "requests", "scikit-learn",
                     "torch", "torchvision", "transformers==5.16.1", "ultralytics==8.4.185",
                     "open_clip_torch==3.3.0")
.fr_py_modules <- c("numpy", "pandas", "cv2", "PIL", "requests", "torch", "transformers", "ultralytics",
                    "open_clip")
.fr_ua <- "FruitTrait R package (https://github.com/ehsanrahimi666/FruitTrait)"
.fr_inat_key <- "50c9509d-22c7-4a22-a47d-8c48425ef4a7"

# Data sets of the package, also when it is loaded but not attached.
.fr_data <- function(name) get(name, envir = getNamespaceInfo("FruitTrait", "lazydata"))

`%||%` <- function(a, b) if (is.null(a)) b else a

.fr_lgl <- function(v) {
  if (is.logical(v)) return(v)
  s <- toupper(trimws(as.character(v)))
  out <- s %in% c("TRUE", "T", "1", "1.0")
  out[is.na(v) | s == "" | s == "NA" | s == "NAN"] <- NA
  out
}

.fr_with_seed <- function(seed, code) {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = globalenv()))
  } else {
    on.exit(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv()))
  }
  set.seed(seed)
  code
}
