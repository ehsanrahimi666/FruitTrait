#' Install the 'Python' back end
#'
#' Creates a 'Python' environment and installs the packages that the photo back
#' end needs: 'PyTorch', 'transformers' (Grounding DINO), 'ultralytics' (SAM
#' 2.1), 'open_clip_torch', 'scikit-learn' and 'OpenCV', in the versions used to
#' build the published data set. This is needed only for [fr_process()];
#' everything else in \pkg{FruitTrait} runs in R alone. Installation downloads
#' about 2-3 GB, and the first run downloads the model weights (about 1.5 GB).
#'
#' @param envname Name of the environment.
#' @param method `"virtualenv"` (default) or `"conda"`.
#' @param python Optional path to the 'Python' executable used to create a
#'   virtual environment (version 3.9 or later).
#' @param cpu_only If `TRUE`, install the CPU build of 'PyTorch' (smaller; no
#'   GPU support).
#' @param pip_options Extra options passed to `pip install`.
#' @return `envname`, invisibly.
#' @seealso [fr_use_backend()], [fr_backend_available()]
#' @examples
#' \dontrun{
#' fr_install()                 # once
#' fr_install(cpu_only = TRUE)  # computer without an NVIDIA GPU
#' }
#' @export
fr_install <- function(envname = "r-FruitTrait", method = c("virtualenv", "conda"),
                       python = NULL, cpu_only = FALSE, pip_options = character()) {
  method <- match.arg(method)
  pkgs <- .fr_py_packages
  torch_cpu <- c("--index-url", "https://download.pytorch.org/whl/cpu")
  if (method == "virtualenv") {
    if (!reticulate::virtualenv_exists(envname)) {
      args <- list(envname = envname)
      if (!is.null(python)) args$python <- python
      do.call(reticulate::virtualenv_create, args)
    }
    if (cpu_only) {
      reticulate::virtualenv_install(envname, c("torch", "torchvision"), pip_options = torch_cpu)
      pkgs <- setdiff(pkgs, c("torch", "torchvision"))
    }
    reticulate::virtualenv_install(envname, pkgs, pip_options = pip_options)
  } else {
    if (!envname %in% reticulate::conda_list()$name) reticulate::conda_create(envname, python_version = "3.11")
    if (cpu_only) {
      reticulate::conda_install(envname, c("torch", "torchvision"), pip = TRUE, pip_options = torch_cpu)
      pkgs <- setdiff(pkgs, c("torch", "torchvision"))
    }
    reticulate::conda_install(envname, pkgs, pip = TRUE, pip_options = pip_options)
  }
  message("Installed. In a new R session call fr_use_backend(\"", envname, "\").")
  invisible(envname)
}

#' Select and check the 'Python' back end
#'
#' `fr_use_backend()` selects the environment made by [fr_install()] for the
#' rest of the R session (it sets the option `FruitTrait.python`).
#' `fr_backend_available()` reports whether all back-end modules can be
#' imported. The back end runs in a separate 'Python' process, so 'PyTorch' is
#' never loaded into the R session.
#'
#' @inheritParams fr_install
#' @param python Path to a 'Python' executable with the back-end packages
#'   (overrides `envname`).
#' @return `fr_use_backend()`: the path of the 'Python' executable, invisibly.
#'   `fr_backend_available()`: `TRUE` or `FALSE`.
#' @examples
#' \donttest{
#' fr_backend_available()
#' }
#' \dontrun{
#' fr_use_backend()
#' }
#' @export
fr_use_backend <- function(envname = "r-FruitTrait", method = c("virtualenv", "conda"), python = NULL) {
  method <- match.arg(method)
  if (is.null(python)) {
    python <- if (method == "virtualenv") reticulate::virtualenv_python(envname) else
      reticulate::conda_python(envname)
  }
  if (!file.exists(python)) stop("Python not found: ", python, call. = FALSE)
  options(FruitTrait.python = python)
  invisible(python)
}

#' @rdname fr_use_backend
#' @export
fr_backend_available <- function(python = NULL) {
  py <- tryCatch(.fr_python(python), error = function(e) "")
  if (!nzchar(py) || !file.exists(py)) return(FALSE)
  code <- paste0("import ", paste(.fr_py_modules, collapse = ", "))
  st <- suppressWarnings(system2(py, c("-c", shQuote(code)), stdout = FALSE, stderr = FALSE))
  identical(as.integer(st), 0L)
}

#' Process fruit photos with the back end
#'
#' Runs the photo pipeline of the FruitTrait data set on each photo:
#' \enumerate{
#'   \item 'Grounding DINO' (base) with the prompt "fruit. berry." on the whole
#'     photo (reduced to 480-800 px) finds the fruits; the display is the box
#'     around the best fruit and the confident fruits near it;
#'   \item a square crop with a 15% margin is taken at native resolution
#'     (longer side at most 1024 px) and 'Grounding DINO' runs again inside it
#'     (two-stage detection); boxes enclosing two or more smaller boxes are
#'     clusters and are set aside;
#'   \item 'SAM' 2.1-B outlines every remaining fruit (up to 12);
#'   \item each outline is checked: at least 800 pixels (about 32 px across),
#'     filling at least 35% of its box, compact (solidity at least 0.80), not
#'     cut by the crop border and not overlapping other fruits (at most 20%);
#'     with three or more fruits, a fruit more than six times larger than the
#'     median of the others is rejected as an undetected cluster;
#'   \item the fruit heads (logistic regressions on 'OpenCLIP' embeddings of the
#'     fruit in context and alone, with outline and colour metrics) give the
#'     probabilities that the outline is a single fruit with a correct outline
#'     (`p_valid`), that the fruit is ripe (`p_ripe`) and that it is dry
#'     (`p_dry`); a fruit is measurable when `p_valid` and `p_ripe` are at
#'     least 0.5;
#'   \item the CIELAB colour of each fruit is the mean of its pixels without
#'     the brightest 2%; the colour of a photo (`m_L`, `m_a`, `m_b`, ...) is the
#'     mean of its measurable fruits.
#' }
#' Results are written to `out_dir` (`traits.csv`, `fruits.csv`, `crops/`,
#' `masks/`, `fruits/` label maps, `overlays/`, and `photos/` for downloaded
#' photos) and read back with [fr_read_results()]. Runs can be resumed: photos
#' already processed are skipped.
#'
#' The method was built for single fleshy fruits of woody plants
#' ([fr_in_scope()]). Fruit heads are part of the back end from the version that
#' accompanies the data set; without them, fruits are outlined and checked but
#' not classified, and `measurable` is `NA`.
#'
#' @param photos Photos to process: one photo (file path or URL), a character
#'   vector of paths or URLs, a folder (searched recursively; photos in
#'   sub-folders named after species get that species, see [fr_read_folder()]),
#'   or a data frame with a `url` or `path` column (for example from
#'   [fr_inat_photos()] or [fr_read_folder()]). Other columns such as
#'   `photo_id` and `species` are kept.
#' @param out_dir Output folder.
#' @param two_stage Run the second detection inside the crop (default, as in
#'   the data set). `FALSE` is faster but finds fewer small fruits.
#' @param heads Optional fruit-heads file (JSON); the default is the file
#'   shipped with the back end.
#' @param embeddings Save the fruit feature sets of each photo
#'   (`embeddings/<photo_id>.npz`), for example to train new heads.
#' @param limit Optional maximum number of photos (for a test run).
#' @param rule Decision rule applied to the results; see [fr_filter()].
#' @param threads Optional number of CPU threads for 'PyTorch'.
#' @param python Optional path to the 'Python' executable; see
#'   [fr_use_backend()].
#' @return A `fr_results` data frame (see [fr_read_results()]).
#' @seealso [fr_install()], [fr_filter()], [fr_contact_sheet()]
#' @examples
#' \dontrun{
#' fr_use_backend()
#' ex <- fr_example_photos()
#' res <- fr_process(ex$photo, out_dir = file.path(tempdir(), "fr_run"))
#' summary(res)
#'
#' ph <- fr_inat_photos("Sorbus aucuparia", n = 20)
#' res <- fr_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")
#' }
#' @export
fr_process <- function(photos, out_dir, two_stage = TRUE, heads = NULL, embeddings = FALSE, limit = NULL,
                       rule = "measurable", threads = NULL, python = NULL) {
  items <- .fr_items(photos)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  inp <- file.path(out_dir, "input.csv")
  utils::write.csv(items, inp, row.names = FALSE)
  argv <- c("--list", normalizePath(inp), "--out", normalizePath(out_dir))
  if (!two_stage) argv <- c(argv, "--one-stage")
  if (!is.null(heads)) argv <- c(argv, "--heads", normalizePath(heads))
  if (embeddings) argv <- c(argv, "--embeddings")
  if (!is.null(threads)) argv <- c(argv, "--threads", as.character(as.integer(threads)))
  if (!is.null(limit)) argv <- c(argv, "--limit", as.character(as.integer(limit)))
  cache <- tools::R_user_dir("FruitTrait", "cache")
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  .fr_run_python("fruittrait.run_photos", argv, python, wd = cache)
  fr_read_results(out_dir, rule = rule)
}

.fr_python <- function(python = NULL) {
  py <- python %||% getOption("FruitTrait.python") %||% Sys.getenv("FRUITTRAIT_PYTHON", "")
  if (!nzchar(py)) py <- reticulate::py_config()$python
  py
}

# Runs a back-end module in a separate Python process (PyTorch is never loaded into the R session). The working
# directory is the cache folder, where the SAM weights are downloaded on first use.
.fr_run_python <- function(module, args, python = NULL, wd = NULL) {
  py <- .fr_python(python)
  if (!file.exists(py)) stop("Python not found: ", py, ". See fr_install().", call. = FALSE)
  old <- Sys.getenv("PYTHONPATH", unset = NA)
  pp <- system.file("python", package = "FruitTrait")
  Sys.setenv(PYTHONPATH = if (is.na(old) || !nzchar(old)) pp else paste(pp, old, sep = .Platform$path.sep))
  on.exit(if (is.na(old)) Sys.unsetenv("PYTHONPATH") else Sys.setenv(PYTHONPATH = old), add = TRUE)
  if (!is.null(wd)) {
    owd <- setwd(wd)
    on.exit(setwd(owd), add = TRUE)
  }
  st <- system2(py, c("-m", module, shQuote(args)))
  if (!identical(as.integer(st), 0L)) {
    stop("The Python back end stopped with status ", st, "; see the messages above.", call. = FALSE)
  }
  invisible(TRUE)
}

.fr_items <- function(photos) {
  if (is.data.frame(photos)) {
    src <- if ("path" %in% names(photos) && any(!is.na(photos$path))) "path" else "url"
    if (!src %in% names(photos)) stop("`photos` needs a `url` or `path` column.", call. = FALSE)
    items <- photos
    items$path <- photos[[src]]
    if (!"photo_id" %in% names(items)) items$photo_id <- .fr_ids(items$path)
    items <- items[!is.na(items$path), , drop = FALSE]
  } else if (is.character(photos) && length(photos) == 1L && dir.exists(photos)) {
    items <- fr_read_folder(photos)
  } else if (is.character(photos)) {
    local <- !grepl("^https?://", photos)
    if (any(local & !file.exists(photos))) {
      stop("Files not found: ", paste(photos[local & !file.exists(photos)], collapse = ", "), call. = FALSE)
    }
    p <- photos
    p[local] <- normalizePath(p[local])
    items <- data.frame(photo_id = .fr_ids(photos), path = p, stringsAsFactors = FALSE)
  } else {
    stop("`photos` must be a folder, file paths, URLs or a data frame.", call. = FALSE)
  }
  if (anyDuplicated(items$photo_id)) stop("`photo_id` values must be unique.", call. = FALSE)
  items$photo_id <- as.character(items$photo_id)
  remote <- grepl("^https?://", items$path)
  items$photo_url <- ifelse(remote, items$path, NA_character_)
  items$path[remote] <- NA_character_
  items[, c("photo_id", "path", "photo_url", setdiff(names(items), c("photo_id", "path", "photo_url"))),
        drop = FALSE]
}

.fr_ids <- function(p) {
  tools::file_path_sans_ext(basename(sub("\\?.*$", "", p)))
}
