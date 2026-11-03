#' Look up trait definitions and reliability
#'
#' Returns rows of [fr_catalogue]: the definition, level (fruit or photo),
#' trait family, unit, how the trait is computed, its photo-to-photo
#' repeatability, its invariance to camera changes, the smallest fruit size at
#' which it stayed stable, and the reliability verdict.
#'
#' @param trait Optional trait names.
#' @param level Optional level: `"fruit"` or `"photo"`.
#' @param family Optional trait family, for example `"colour"`, `"classifier"`
#'   or `"size and shape"`.
#' @param verdict Optional verdict: `"reliable"`, `"use with care"` or
#'   `"not reliable"`.
#' @param columns Columns to return; `NULL` returns all.
#' @return A data frame.
#' @examples
#' fr_trait_info(c("m_L", "m_C", "m_hue"))
#' fr_trait_info(level = "photo", verdict = "reliable")
#' table(fr_catalogue$family, fr_catalogue$verdict)
#' @export
fr_trait_info <- function(trait = NULL, level = NULL, family = NULL, verdict = NULL,
                          columns = c("trait", "level", "family", "unit", "verdict", "reason",
                                      "icc_measurable", "inv_photometric_min", "min_D_px",
                                      "computed_by", "description")) {
  ct <- .fr_data("fr_catalogue")
  x <- ct
  if (!is.null(trait)) {
    bad <- setdiff(trait, x$trait)
    if (length(bad)) stop("Unknown trait: ", paste(bad, collapse = ", "), call. = FALSE)
    x <- x[match(trait, x$trait), ]
  }
  if (!is.null(level)) x <- x[x$level %in% level, ]
  if (!is.null(family)) x <- x[x$family %in% family, ]
  if (!is.null(verdict)) x <- x[as.character(x$verdict) %in% verdict, ]
  if (!is.null(columns)) {
    bad <- setdiff(columns, names(ct))
    if (length(bad)) stop("Unknown column: ", paste(bad, collapse = ", "), call. = FALSE)
    x <- x[, columns, drop = FALSE]
  }
  rownames(x) <- NULL
  x
}

#' Names of reliable traits
#'
#' Selects traits by their validation verdict and, optionally, by level,
#' family and the smallest fruit size at which they stayed stable (ICC of at
#' least 0.8 against the full-resolution value).
#'
#' @param verdict Verdicts to include.
#' @param level Level: `"photo"` (default), `"fruit"` or both.
#' @param family Optional trait family.
#' @param fruit_px Optional typical fruit size in your photos (equivalent
#'   diameter, pixels). Traits whose minimum size is larger, or unknown, are
#'   dropped.
#' @return A character vector of trait names.
#' @examples
#' fr_reliable_traits()
#' fr_reliable_traits(c("reliable", "use with care"), level = "fruit")
#' fr_reliable_traits(fruit_px = 16)
#' @export
fr_reliable_traits <- function(verdict = "reliable", level = "photo", family = NULL, fruit_px = NULL) {
  ct <- .fr_data("fr_catalogue")
  x <- ct[as.character(ct$verdict) %in% verdict & ct$level %in% level, ]
  if (!is.null(family)) x <- x[x$family %in% family, ]
  if (!is.null(fruit_px)) x <- x[!is.na(x$min_D_px) & x$min_D_px <= fruit_px, ]
  x$trait
}
