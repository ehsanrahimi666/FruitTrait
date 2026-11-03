#' Read a photo
#'
#' Reads a JPEG or PNG file into an RGB array with values from 0 to 255.
#' Grey-scale images are expanded to three channels and an alpha channel is
#' dropped. The file type is taken from the file signature, not the extension.
#'
#' @param path Path to a JPEG or PNG file.
#' @param max_side Optional maximum length (pixels) of the longer side. Larger
#'   images are reduced by area averaging, as in the back end.
#' @return A numeric array with dimensions height x width x 3 (values 0-255).
#' @seealso [fr_read_labels()], [fr_fruit_colour()]
#' @examples
#' ex <- fr_example_photos()
#' img <- fr_read_image(ex$crop[1])
#' dim(img)
#' @export
fr_read_image <- function(path, max_side = NULL) {
  x <- .fr_read_raw(path)
  if (length(dim(x)) == 2L) x <- array(x, c(dim(x), 3L))
  if (dim(x)[3] == 2L) x <- x[, , c(1, 1, 1), drop = FALSE]
  if (dim(x)[3] == 4L) x <- x[, , 1:3, drop = FALSE]
  img <- round(x * 255)
  if (!is.null(max_side)) img <- .fr_shrink(img, max_side)
  img
}

#' Read fruit label maps and masks
#'
#' The back end writes, for every photo, a label map of the fruits that passed
#' the outline checks (`fruits/<photo_id>.png`: 0 = background, 1, 2, ... =
#' fruit number `label` of [fr_fruits()]) and the mask of all of them
#' (`masks/<photo_id>.png`). `fr_read_labels()` returns the integer label map,
#' `fr_read_mask()` a logical mask (any non-zero pixel), and `fr_write_mask()`
#' writes a logical mask as a PNG with values 0 and 255.
#'
#' @param path Path to a PNG file.
#' @param mask Logical (or 0/1) matrix.
#' @return `fr_read_labels()`: an integer matrix. `fr_read_mask()`: a logical
#'   matrix. `fr_write_mask()`: `path`, invisibly.
#' @examples
#' ex <- fr_example_photos()
#' lab <- fr_read_labels(ex$labels[1])
#' table(lab[lab > 0])
#' m <- fr_read_mask(ex$labels[1])
#' f <- tempfile(fileext = ".png")
#' fr_write_mask(m, f)
#' identical(fr_read_mask(f), m)
#' @export
fr_read_labels <- function(path) {
  if (.fr_format(path) != "png") stop("Label maps are PNG files: ", path, call. = FALSE)
  x <- png::readPNG(path)
  if (length(dim(x)) == 3L) x <- x[, , 1]
  out <- round(x * 255)
  storage.mode(out) <- "integer"
  out
}

#' @rdname fr_read_labels
#' @export
fr_read_mask <- function(path) {
  x <- .fr_read_raw(path)
  if (length(dim(x)) == 3L) x <- x[, , 1]
  if (.fr_format(path) == "jpeg") x > 0.5 else x > 0
}

#' @rdname fr_read_labels
#' @export
fr_write_mask <- function(mask, path) {
  if (!is.matrix(mask)) stop("`mask` must be a matrix.", call. = FALSE)
  png::writePNG(matrix(as.numeric(mask != 0), nrow(mask)), path)
  invisible(path)
}

#' Example fruit photos shipped with the package
#'
#' Research-grade 'iNaturalist' photos (CC0 or CC BY 4.0) of fruiting woody
#' plants, reduced to 1024 pixels on the longer side, with the crops and fruit
#' label maps produced by the back end. The table gives the rights holder and
#' licence of each photo; please keep this attribution when you reuse them.
#'
#' @return A data frame with one row per photo: `photo_id`, `species`,
#'   `family`, `latitude`, `longitude`, `observation_url`, `rights_holder`,
#'   `license` and the full paths `photo`, `crop` and `labels`.
#' @seealso [fr_example_results()]
#' @examples
#' ex <- fr_example_photos()
#' ex[, c("photo_id", "species", "license", "rights_holder")]
#' @export
fr_example_photos <- function() {
  d <- system.file("extdata", package = "FruitTrait")
  x <- utils::read.csv(file.path(d, "ATTRIBUTION.csv"), stringsAsFactors = FALSE,
                       colClasses = c(photo_id = "character"))
  x$photo <- file.path(d, "photos", paste0(x$photo_id, ".jpg"))
  x$crop <- file.path(d, "crops", paste0(x$photo_id, ".jpg"))
  x$labels <- file.path(d, "fruits", paste0(x$photo_id, ".png"))
  x
}

#' Back-end results for the example photos
#'
#' The output of [fr_process()] for the photos in [fr_example_photos()], read
#' with [fr_read_results()], so that filtering, summaries and plots can be
#' tried without 'Python'.
#'
#' @inheritParams fr_filter
#' @return A `fr_results` data frame (see [fr_read_results()]).
#' @examples
#' res <- fr_example_results()
#' res[, c("photo_id", "species", "n_fruits_ok", "keep")]
#' @export
fr_example_results <- function(rule = "outlined") {
  fr_read_results(system.file("extdata", package = "FruitTrait"), rule = rule)
}

.fr_format <- function(path) {
  sig <- readBin(path, "raw", n = 8L)
  if (length(sig) >= 3L && identical(sig[1:3], as.raw(c(0xFF, 0xD8, 0xFF)))) return("jpeg")
  png_sig <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  if (length(sig) == 8L && identical(sig, png_sig)) return("png")
  "other"
}

.fr_read_raw <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("File not found: ", paste(path, collapse = ", "), call. = FALSE)
  }
  switch(.fr_format(path),
         jpeg = jpeg::readJPEG(path),
         png = png::readPNG(path),
         stop("Only JPEG and PNG files are supported: ", path, call. = FALSE))
}

.fr_as_image <- function(img) {
  if (is.character(img)) img <- fr_read_image(img)
  d <- dim(img)
  if (!is.numeric(img) || length(d) != 3L || d[3] != 3L) {
    stop("`img` must be a height x width x 3 array (values 0-255) or a file path.", call. = FALSE)
  }
  if (max(img, na.rm = TRUE) <= 1 && any(img > 0)) {
    warning("`img` looks scaled to 0-1; FruitTrait expects 0-255.", call. = FALSE)
  }
  img
}

.fr_as_labels <- function(labels) {
  if (is.character(labels)) labels <- fr_read_labels(labels)
  if (is.logical(labels)) labels <- labels * 1L
  if (!is.matrix(labels)) stop("`labels` must be a matrix or a PNG file path.", call. = FALSE)
  labels[is.na(labels)] <- 0L
  labels
}

# Area-averaging resize along the first dimension (exact pixel-overlap weights from cumulative sums).
.fr_area_rows <- function(M, n_out) {
  n_in <- nrow(M)
  if (n_out == n_in) return(M)
  s <- n_in / n_out
  C <- rbind(0, apply(M, 2, cumsum))
  e <- (0:n_out) * s
  k <- pmin(floor(e), n_in - 1)
  S <- C[k + 1, , drop = FALSE] + (e - k) * M[k + 1, , drop = FALSE]
  (S[-1, , drop = FALSE] - S[-(n_out + 1), , drop = FALSE]) / s
}

.fr_area_resize <- function(M, h, w) {
  t(.fr_area_rows(t(.fr_area_rows(M, h)), w))
}

.fr_shrink <- function(img, max_side) {
  s <- max_side / max(dim(img)[1:2])
  if (s >= 1) return(img)
  h <- max(1L, round(dim(img)[1] * s))
  w <- max(1L, round(dim(img)[2] * s))
  out <- array(0, c(h, w, 3L))
  for (ch in 1:3) out[, , ch] <- .fr_area_resize(img[, , ch], h, w)
  round(out)
}

# Label map reduced to at most max_side (nearest label of the source pixel).
.fr_labels_fit <- function(lab, max_side) {
  s <- max_side / max(dim(lab))
  if (s >= 1) return(lab)
  h <- max(1L, round(nrow(lab) * s))
  w <- max(1L, round(ncol(lab) * s))
  lab[pmin(nrow(lab), floor((seq_len(h) - 0.5) / s) + 1), pmin(ncol(lab), floor((seq_len(w) - 0.5) / s) + 1),
      drop = FALSE]
}
