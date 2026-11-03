#' Plot a crop with its fruit outlines
#'
#' Draws a crop and the outline of every fruit of its label map. With a fruit
#' table, measurable fruits are drawn in yellow and the other outlined fruits in
#' white, each with its label number.
#'
#' @param img Crop: RGB array (values 0-255) or file path.
#' @param labels Label map (integer matrix) or PNG file path; `NULL` plots the
#'   crop only.
#' @param fruits Optional fruit rows of this photo (from [fr_fruits()]).
#' @param lwd Outline width.
#' @param numbers Write the label number next to each fruit.
#' @param main Optional title.
#' @return `NULL`, invisibly.
#' @examples
#' ex <- fr_example_photos()
#' fr_plot_fruits(ex$crop[1], ex$labels[1], main = ex$species[1])
#' @export
fr_plot_fruits <- function(img, labels = NULL, fruits = NULL, lwd = 2, numbers = TRUE, main = NULL) {
  img <- .fr_as_image(img)
  lab <- if (is.null(labels)) NULL else .fr_as_labels(labels)
  .fr_panel(img, lab, fruits, lwd, numbers, main, font = 3)
}

.fr_panel <- function(img, lab, fruits, lwd, numbers, main, font = 1, cex = 0.95) {
  h <- dim(img)[1]
  w <- dim(img)[2]
  op <- graphics::par(mar = c(0.2, 0.2, if (is.null(main)) 0.2 else 1.6, 0.2))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, w), ylim = c(0, h), asp = 1, xaxs = "i", yaxs = "i")
  graphics::rasterImage(grDevices::as.raster(img / 255), 0, 0, w, h, interpolate = FALSE)
  if (!is.null(lab)) {
    for (k in sort(setdiff(unique(as.vector(lab)), 0L))) {
      meas <- if (!is.null(fruits) && "measurable" %in% names(fruits)) {
        isTRUE(fruits$measurable[match(k, fruits$label)])
      } else TRUE
      m <- lab == k
      .fr_draw_outline(m, if (meas) "#FFD400" else "#FFFFFF", lwd)
      if (numbers) {
        idx <- which(m, arr.ind = TRUE)
        graphics::text(mean(idx[, 2]) - 0.5, h - mean(idx[, 1]) + 0.5, k, col = "white", font = 2, cex = 0.8)
      }
    }
  }
  if (!is.null(main)) graphics::title(main, cex.main = cex, font.main = font, line = 0.4)
  invisible(NULL)
}

.fr_draw_outline <- function(m, col, lwd) {
  h <- nrow(m)
  w <- ncol(m)
  z <- matrix(0, w + 2, h + 2)
  z[2:(w + 1), 2:(h + 1)] <- t(m[h:1, , drop = FALSE])
  cl <- grDevices::contourLines(x = (0:(w + 1)) - 0.5, y = (0:(h + 1)) - 0.5, z = z, levels = 0.5)
  for (k in cl) graphics::lines(k$x, k$y, col = col, lwd = lwd)
}

#' Contact sheet of crops and fruit outlines
#'
#' Draws a grid of crops with the outlines of their fruits (yellow: measurable;
#' white: other outlined fruits), labelled with a column of the results, to
#' check a run by eye.
#'
#' @param x A `fr_results` data frame.
#' @param dir Results folder with `crops/` and `fruits/`; defaults to
#'   `attr(x, "dir")`.
#' @param n Maximum number of photos (a random sample if `x` has more rows).
#' @param ncol Number of columns.
#' @param label Column used as the title of each crop.
#' @param file Optional PNG file to write instead of the current device.
#' @param seed Random seed for the sample.
#' @return The rows of `x` that were drawn, invisibly.
#' @examples
#' res <- fr_example_results()
#' fr_contact_sheet(res, ncol = 3)
#' @export
fr_contact_sheet <- function(x, dir = attr(x, "dir"), n = 24, ncol = 6, label = "species", file = NULL, seed = 1) {
  if (is.null(dir)) stop("Give the results folder in `dir`.", call. = FALSE)
  crop <- file.path(dir, "crops", paste0(x$photo_id, ".jpg"))
  labs <- file.path(dir, "fruits", paste0(x$photo_id, ".png"))
  ok <- file.exists(crop) & file.exists(labs)
  fr <- attr(x, "fruits")
  x <- x[ok, , drop = FALSE]
  crop <- crop[ok]
  labs <- labs[ok]
  if (!nrow(x)) stop("No crops and label maps found in ", dir, call. = FALSE)
  if (nrow(x) > n) {
    i <- sort(.fr_with_seed(seed, sample(nrow(x), n)))
    x <- x[i, , drop = FALSE]
    crop <- crop[i]
    labs <- labs[i]
  }
  ncol <- min(ncol, nrow(x))
  nrow_ <- ceiling(nrow(x) / ncol)
  if (!is.null(file)) {
    grDevices::png(file, width = 300 * ncol, height = 330 * nrow_, res = 120)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  op <- graphics::par(mfrow = c(nrow_, ncol), oma = c(0, 0, 0, 0))
  on.exit(graphics::par(op), add = TRUE)
  ttl <- if (label %in% names(x)) as.character(x[[label]]) else x$photo_id
  for (i in seq_len(nrow(x))) {
    img <- fr_read_image(crop[i])
    lab <- fr_read_labels(labs[i])
    s <- 400 / max(dim(img)[1:2])
    if (s < 1) {
      img <- .fr_shrink(img, 400)
      lab <- .fr_labels_fit(lab, 400)
    }
    rows <- if (is.null(fr)) NULL else fr[fr$photo_id == x$photo_id[i] & fr$ok %in% TRUE, , drop = FALSE]
    .fr_panel(img, lab, rows, 1.6, FALSE, ttl[i], font = if (label == "species") 3 else 1, cex = 0.7)
  }
  invisible(x)
}

#' Plot fruit colours
#'
#' Draws the colour of every measurable fruit (or of every outlined fruit) as a
#' swatch, one row per group, with groups ordered by their mean lightness.
#'
#' @param x A `fr_results` data frame.
#' @param group Column of the photo table used to group fruits (default
#'   `"species"`).
#' @param which Fruits to draw: `"measurable"` (default when fruit heads were
#'   used) or `"outlined"`.
#' @param max_per_group Maximum swatches per group.
#' @return The fruit table used, invisibly.
#' @examples
#' res <- fr_example_results()
#' fr_plot_colours(res, which = "outlined")
#' @export
fr_plot_colours <- function(x, group = "species", which = c("measurable", "outlined"), max_per_group = 40) {
  which <- match.arg(which)
  f <- fr_fruits(x, which)
  if (!nrow(f) && which == "measurable") {
    f <- fr_fruits(x, "outlined")
    which <- "outlined"
  }
  if (!nrow(f)) stop("No fruits to plot.", call. = FALSE)
  g <- if (group %in% names(f)) as.character(f[[group]]) else
    as.character(x[[group]] %||% x$photo_id)[match(f$photo_id, x$photo_id)]
  g[is.na(g)] <- "(unknown)"
  f$col <- .fr_lab_to_hex(cbind(f$L, f$a, f$b))
  ord <- names(sort(tapply(f$L, g, mean)))
  n <- min(max_per_group, max(table(g)))
  op <- graphics::par(mar = c(2, max(6, 0.5 * max(nchar(ord))), 1.5, 1))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, n), ylim = c(0, length(ord)))
  for (j in seq_along(ord)) {
    cols <- utils::head(f$col[g == ord[j]][order(f$L[g == ord[j]])], n)
    graphics::rect(seq_along(cols) - 1, j - 0.9, seq_along(cols) - 0.05, j - 0.1, col = cols, border = NA)
  }
  graphics::axis(2, at = seq_along(ord) - 0.5, labels = ord, las = 1, tick = FALSE, font = 3, cex.axis = 0.8)
  graphics::title(main = paste0("Fruit colours (", which, " fruits)"), cex.main = 0.9, font.main = 1)
  invisible(f)
}

#' Plot repeatability estimates
#'
#' Draws the ICC of each trait with its bootstrap interval, as returned by
#' [fr_repeatability()], ordered by the estimate.
#'
#' @param r Data frame from [fr_repeatability()].
#' @param threshold Reference line (the repeatability threshold of the
#'   reliability verdicts in [fr_catalogue]).
#' @return `r`, invisibly, ordered by the ICC.
#' @examples
#' v <- fr_validation[fr_validation$both_measurable, ]
#' r <- fr_repeatability(v, traits = c("m_L", "m_a", "m_b", "m_C"), boot = 50)
#' fr_plot_repeatability(r)
#' @export
fr_plot_repeatability <- function(r, threshold = 0.6) {
  need <- c("trait", "icc", "lower", "upper")
  if (!is.data.frame(r) || !all(need %in% names(r))) {
    stop("`r` must come from fr_repeatability().", call. = FALSE)
  }
  r <- r[order(r$icc), , drop = FALSE]
  n <- nrow(r)
  op <- graphics::par(mar = c(4, 9, 1, 1))
  on.exit(graphics::par(op))
  lo <- min(0, r$lower, r$icc, na.rm = TRUE)
  graphics::plot(r$icc, seq_len(n), xlim = c(lo, 1), ylim = c(0.5, n + 0.5), yaxt = "n",
                 pch = 19, col = "#1F3B66", xlab = "Photo-to-photo ICC", ylab = "")
  graphics::segments(r$lower, seq_len(n), r$upper, seq_len(n), col = "#1F3B66")
  graphics::axis(2, at = seq_len(n), labels = r$trait, las = 1, cex.axis = 0.8)
  graphics::abline(v = threshold, lty = 2, col = "grey50")
  invisible(r)
}
