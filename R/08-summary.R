#' Summarise fruit colour by species or another group
#'
#' Averages the colour of measurable fruits per group: first per photo (each
#' fruit weighted equally, as the photo colour `m_*` of the back end), then over
#' the photos of each group (each photo weighted equally). Chroma and hue are
#' computed from the mean a* and b*; the hue concentration (mean resultant
#' length of hue angles weighted by chroma) is also reported.
#'
#' @param x A `fr_results` data frame.
#' @param by Grouping column of the photo table (default `"species"`).
#' @param which Fruits used: `"measurable"` (default) or `"outlined"`.
#' @param min_photos Groups with fewer photos are dropped.
#' @return A data frame with one row per group: `n_photos`, `n_fruits`, `L`,
#'   `a`, `b`, `C`, `hue_deg`, `hue_concentration`, `name` (colour name of the
#'   mean), `L_sd` (SD of the photo means) and `share_<name>` columns (share of
#'   photos whose mean colour has each colour name).
#' @examples
#' res <- fr_example_results()
#' fr_summarise(res, which = "outlined")
#' @export
fr_summarise <- function(x, by = "species", which = c("measurable", "outlined"), min_photos = 1) {
  which <- match.arg(which)
  f <- fr_fruits(x, which)
  if (!by %in% names(x)) stop("Column not found in the results: ", by, call. = FALSE)
  if (!nrow(f)) stop("No ", which, " fruits.", call. = FALSE)
  ph <- stats::aggregate(cbind(L = f$L, a = f$a, b = f$b), list(photo_id = f$photo_id), mean)
  ph$n_fruits <- as.vector(table(f$photo_id)[ph$photo_id])
  ph$group <- as.character(x[[by]])[match(ph$photo_id, x$photo_id)]
  ph <- ph[!is.na(ph$group), , drop = FALSE]
  ph$name <- fr_colour_name(ph$L, ph$a, ph$b)
  rows <- lapply(split(ph, ph$group), function(d) {
    L <- mean(d$L)
    a <- mean(d$a)
    b <- mean(d$b)
    C <- sqrt(d$a^2 + d$b^2)
    h <- atan2(d$b, d$a)
    conc <- if (sum(C) > 0) sqrt(sum(C * cos(h))^2 + sum(C * sin(h))^2) / sum(C) else NA_real_
    out <- data.frame(group = d$group[1], n_photos = nrow(d), n_fruits = sum(d$n_fruits), L = L, a = a, b = b,
                      C = sqrt(a^2 + b^2), hue_deg = (atan2(b, a) * 180 / pi) %% 360, hue_concentration = conc,
                      name = fr_colour_name(L, a, b), L_sd = if (nrow(d) > 1) stats::sd(d$L) else NA_real_,
                      stringsAsFactors = FALSE)
    tab <- table(factor(d$name, levels = c("white", "grey", "black", "red", "pink", "orange", "yellow", "brown",
                                            "green", "blue", "purple")))
    sh <- as.list(as.numeric(tab) / nrow(d))
    names(sh) <- paste0("share_", names(tab))
    cbind(out, as.data.frame(sh))
  })
  out <- do.call(rbind, rows)
  out <- out[out$n_photos >= min_photos, , drop = FALSE]
  names(out)[1] <- by
  rownames(out) <- NULL
  out
}
