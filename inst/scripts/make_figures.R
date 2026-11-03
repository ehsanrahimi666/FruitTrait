# Figures of the FruitTrait article, made from the data and photographs shipped with the package.
# Usage: Rscript make_figures.R OUTPUT_FOLDER
# Photographs: CC0 / CC BY photographs from iNaturalist via GBIF; attribution in inst/extdata/ATTRIBUTION.csv and
# inst/extdata/gallery/ATTRIBUTION.csv. Uses the 'maps' package for the world map of Figure 2 (an empty frame is
# drawn when it is not installed).
library(FruitTrait)

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "figures"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------- style
ink <- "#1D2731"; ink2 <- "#5B6770"; grid_col <- "#E6E9EC"
c_keep <- "#2E7D5B"; c_rej <- "#B5443B"; c_hi <- "#E8A33D"; c_mid <- "#3E6A9E"; c_light <- "#DDE7F2"
verdict_col <- c("reliable" = "#2E7D5B", "use with care" = "#E8A33D", "not reliable" = "#B5443B",
                 "not tested" = "#9AA5B1")
head_col <- c(valid = "#3E6A9E", ripe = "#B5443B", dry = "#8C7B5A")
name_col <- c(white = "#F4F4F4", grey = "#9E9E9E", black = "#262626", red = "#D7301F", pink = "#F28CB1",
              orange = "#F28E2B", brown = "#8C5A3C", yellow = "#F6D32D", green = "#4DAF4A", blue = "#4C72B0",
              purple = "#8E5DB5")

fig <- function(name, w, h, expr) {
  grDevices::png(file.path(out, name), width = w, height = h, units = "in", res = 300, type = "cairo", bg = "white")
  op <- graphics::par(family = "sans", cex = 0.8, col.axis = ink2, col.lab = ink, fg = ink2, tcl = -0.25,
                      mgp = c(2.1, 0.45, 0), las = 1, bg = "white")
  on.exit({graphics::par(op); grDevices::dev.off()})
  force(expr)
  invisible(file.path(out, name))
}
plab <- function(x, line = 0.4, adj = -0.02) mtext(x, side = 3, adj = adj, line = line, font = 2, cex = 1.05, col = ink)
ptitle <- function(x, line = 0.35, cex = 0.72) mtext(x, side = 3, line = line, cex = cex, col = ink)
gridlines <- function(h = NULL, v = NULL) abline(h = h, v = v, col = grid_col, lwd = 0.8)
lab_hex <- function(L, a, b) FruitTrait:::.fr_lab_to_hex(cbind(L, a, b))
draw_img <- function(img, border = NA, lwd = 2, square = FALSE) {
  h <- dim(img)[1]; w <- dim(img)[2]
  s <- max(h, w)
  x0 <- if (square) (s - w) / 2 else 0
  y0 <- if (square) (s - h) / 2 else 0
  plot.new()
  if (square) plot.window(c(0, s), c(0, s), asp = 1, xaxs = "i", yaxs = "i") else
    plot.window(c(0, w), c(0, h), asp = 1, xaxs = "i", yaxs = "i")
  rasterImage(as.raster(img / 255), x0, y0, x0 + w, y0 + h)
  if (!is.na(border)) rect(x0, y0, x0 + w, y0 + h, border = border, lwd = lwd, xpd = NA)
  invisible(c(h = h, w = w, x0 = x0, y0 = y0))
}
contour_of <- function(m) {
  h <- nrow(m); w <- ncol(m)
  z <- matrix(0, w + 2, h + 2)
  z[2:(w + 1), 2:(h + 1)] <- t(m[h:1, , drop = FALSE])
  contourLines((0:(w + 1)) - 0.5, (0:(h + 1)) - 0.5, z, levels = 0.5)
}
outline <- function(m, col = "#FFFFFF", lwd = 1.6, halo = TRUE, dx = 0, dy = 0, lty = 1) {
  for (k in contour_of(m)) {
    if (halo) lines(k$x + dx, k$y + dy, col = grDevices::adjustcolor("black", 0.55), lwd = lwd + 1.6)
    lines(k$x + dx, k$y + dy, col = col, lwd = lwd, lty = lty)
  }
}
dim_bg <- function(img, m, f = 0.4) {
  for (k in 1:3) { x <- img[, , k]; x[!m] <- x[!m] * f; img[, , k] <- x }
  img
}
poly_xy <- function(s) {
  p <- do.call(rbind, lapply(strsplit(strsplit(s, ";", fixed = TRUE)[[1]], ",", fixed = TRUE), as.numeric))
  list(x = p[, 1], y = p[, 2])
}
spearman <- function(x, y) stats::cor(x, y, method = "spearman", use = "complete.obs")
roc <- function(p, y) {
  o <- order(-p)
  y <- y[o]
  tp <- cumsum(y) / sum(y)
  fp <- cumsum(!y) / sum(!y)
  auc <- sum(diff(c(0, fp)) * (c(0, tp[-length(tp)]) + tp) / 2)
  list(fp = c(0, fp), tp = c(0, tp), auc = auc)
}
eqearth <- function(lon, lat) {
  A1 <- 1.340264; A2 <- -0.081106; A3 <- 0.000893; A4 <- 0.003796
  n <- max(length(lon), length(lat)); lon <- rep_len(lon, n); lat <- rep_len(lat, n)
  th <- asin(sqrt(3) / 2 * sin(lat * pi / 180))
  x <- 2 * sqrt(3) * (lon * pi / 180) * cos(th) / (3 * (9 * A4 * th^8 + 7 * A3 * th^6 + 3 * A2 * th^2 + A1))
  y <- A4 * th^9 + A3 * th^7 + A2 * th^3 + A1 * th
  list(x = x, y = y)
}

ex <- fr_example_photos()
res <- fr_example_results()
res <- res[match(ex$photo_id, res$photo_id), ]
fru <- fr_fruits(res, "all")
gal_dir <- system.file("extdata", "gallery", package = "FruitTrait")
gal <- utils::read.csv(file.path(gal_dir, "ATTRIBUTION.csv"), colClasses = c(photo_id = "character"))
b <- fr_benchmark
v <- fr_validation
ct <- fr_catalogue
rf <- fr_reference

# ---------------------------------------------------------------- Figure 1: workflow on one photograph
i1 <- which(ex$photo_id == "10402531")
r1 <- res[i1, ]
f1 <- fru[fru$photo_id == ex$photo_id[i1], ]
photo <- fr_read_image(ex$photo[i1]); crop <- fr_read_image(ex$crop[i1]); lab1 <- fr_read_labels(ex$labels[i1])
to_crop <- function(p) list(x = (p$x - r1$crop_x0) * r1$crop_scale, y = dim(crop)[1] - (p$y - r1$crop_y0) * r1$crop_scale)
fig("fig1_workflow.png", 7.2, 6.3, {
  layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 7, 7), 3, byrow = TRUE), heights = c(1, 1, 0.52), widths = c(0.86, 1, 1))
  par(mar = c(0.5, 0.5, 2.2, 0.5))
  sc <- dim(photo)[2] / r1$photo_w
  d <- draw_img(photo)
  rect(r1$crop_x0 * sc, d["h"] - r1$crop_y1 * sc, r1$crop_x1 * sc, d["h"] - r1$crop_y0 * sc, border = "white",
       lwd = 2.2, lty = 2)
  ptitle("Photograph and square crop", line = 0.3); plab("a", adj = 0.01)
  d <- draw_img(crop)
  for (j in seq_len(nrow(f1))) {
    if (f1$ok[j] %in% TRUE) {
      p <- to_crop(poly_xy(f1$outline_xy[j]))
      polygon(p$x, p$y, border = grDevices::adjustcolor("black", 0.55), lwd = 3.2)
      polygon(p$x, p$y, border = "white", lwd = 1.6)
    } else {
      p <- to_crop(list(x = c(f1$x0_photo[j], f1$x1_photo[j]), y = c(f1$y0_photo[j], f1$y1_photo[j])))
      rect(p$x[1], p$y[2], p$x[2], p$y[1], border = grDevices::adjustcolor("black", 0.55), lwd = 3.2)
      rect(p$x[1], p$y[2], p$x[2], p$y[1], border = "#FF5A4E", lwd = 1.6, lty = 2)
    }
  }
  legend("bottomleft", c("outline passed the checks", "outline rejected (box shown)"), col = c("white", "#FF5A4E"),
         lty = c(1, 2), lwd = 1.8, bty = "o", bg = grDevices::adjustcolor("black", 0.55), box.col = NA,
         text.col = "white", cex = 0.72, inset = 0.02)
  ptitle(sprintf("Grounding DINO + SAM 2.1: %d outlines", nrow(f1)), line = 0.3); plab("b", adj = 0.01)
  par(mar = c(3.2, 4.2, 2.2, 1.2))
  ok1 <- f1[f1$ok %in% TRUE, ]
  ok1 <- ok1[order(ok1$label), ]
  yy <- rev(seq_len(nrow(ok1)))
  plot(0, type = "n", xlim = c(0, 1), ylim = c(0.5, nrow(ok1) + 0.5), yaxt = "n", xlab = "Probability", ylab = "")
  gridlines(v = seq(0, 1, 0.25))
  abline(v = 0.5, lty = 2, col = ink2)
  segments(0, yy, 1, yy, col = grid_col)
  points(ok1$p_valid, yy + 0.12, pch = 21, bg = head_col["valid"], col = "white", cex = 1.5)
  points(ok1$p_ripe, yy - 0.12, pch = 24, bg = head_col["ripe"], col = "white", cex = 1.3)
  mtext(paste("fruit", ok1$label), side = 2, at = yy, line = 0.4, las = 1, cex = 0.68,
        col = ifelse(ok1$measurable %in% TRUE, c_keep, c_rej))
  legend("bottomleft", c("p_valid: single fruit, correct outline", "p_ripe: ripe"), pch = c(21, 24),
         pt.bg = head_col[c("valid", "ripe")], col = "white", pt.cex = 1.4, bty = "n", cex = 0.72, text.col = ink,
         inset = c(0, -0.02))
  ptitle("Fruit classifiers", line = 0.3); plab("c", adj = -0.32)
  par(mar = c(0.5, 0.5, 2.2, 0.5))
  meas <- lab1 %in% ok1$label[ok1$measurable %in% TRUE]
  dim(meas) <- dim(lab1)
  d <- draw_img(dim_bg(crop, lab1 > 0))
  outline(meas, c_keep, 2, halo = TRUE)
  outline(lab1 > 0 & !meas, "#FFB000", 1.6, halo = TRUE, lty = 2)
  for (l in ok1$label) {
    m <- lab1 == l
    cy <- d["h"] - mean(row(m)[m]); cx <- mean(col(m)[m])
    text(cx, cy, l, col = "white", font = 2, cex = 0.9)
  }
  ptitle("Measurable fruits (valid and ripe)", line = 0.3); plab("d", adj = 0.01)
  fc <- fr_fruit_colour(crop, lab1)
  par(mar = c(0.5, 0.5, 2.2, 0.5))
  plot.new(); plot.window(c(0, 10), c(0, 10))
  mm <- ok1[ok1$measurable %in% TRUE, ]
  for (j in seq_len(nrow(mm))) {
    x0 <- 0.3 + (j - 1) * 1.9
    rect(x0, 6.6, x0 + 1.6, 8.6, col = lab_hex(mm$L[j], mm$a[j], mm$b[j]), border = "grey60")
    text(x0 + 0.8, 6.1, paste("fruit", mm$label[j]), cex = 0.66, col = ink)
    text(x0 + 0.8, 5.5, mm$name[j], cex = 0.66, col = ink2)
  }
  rect(0.3, 1.2, 3.3, 4.2, col = lab_hex(r1$m_L, r1$m_a, r1$m_b), border = "grey40")
  text(3.7, 3.6, sprintf("photo colour: %s", r1$m_name), adj = 0, cex = 0.8, col = ink)
  text(3.7, 2.5, sprintf("L* %.1f   a* %.1f   b* %.1f", r1$m_L, r1$m_a, r1$m_b), adj = 0, cex = 0.72, col = ink)
  text(3.7, 1.5, sprintf("C* %.1f   hue angle %.0f   n = %d", r1$m_C, r1$m_hue, r1$m_n), adj = 0, cex = 0.72,
       col = ink)
  ptitle("Fruit colour (CIELAB, D65)", line = 0.3); plab("e", adj = 0.01)
  par(mar = c(3.2, 3.4, 2.2, 0.8))
  ag <- merge(fc, ok1[, c("label", "L", "a", "b")], by = "label", suffixes = c("_r", "_py"))
  lim <- range(c(ag$L_r, ag$L_py, ag$a_r, ag$a_py, ag$b_r, ag$b_py)) + c(-3, 3)
  plot(0, type = "n", xlim = lim, ylim = lim, xlab = "Back end (Python)", ylab = "R (fr_fruit_colour)")
  gridlines(h = pretty(lim), v = pretty(lim))
  abline(0, 1, lty = 2, col = ink2)
  points(ag$L_py, ag$L_r, pch = 21, bg = "grey80", col = ink, cex = 1.3)
  points(ag$a_py, ag$a_r, pch = 22, bg = "#D7301F", col = "white", cex = 1.3)
  points(ag$b_py, ag$b_r, pch = 24, bg = "#F6D32D", col = ink2, cex = 1.2)
  legend("topleft", c("L*", "a*", "b*"), pch = c(21, 22, 24), pt.bg = c("grey80", "#D7301F", "#F6D32D"),
         col = c(ink, "white", ink2), pt.cex = 1.3, bty = "n", cex = 0.75, text.col = ink)
  ptitle("Same colour in R and the back end", line = 0.3); plab("f", adj = -0.3)
  par(mar = c(0.2, 0.4, 0.6, 0.4))
  plot.new(); plot.window(c(0, 100), c(0, 40))
  steps <- c("Photo, folder,\nspecies folders,\nGBIF, iNaturalist", "Grounding DINO\n'fruit. berry.'\nwhole photo",
             "Square crop at\nnative resolution", "Grounding DINO\nagain in the crop;\nclusters set aside",
             "SAM 2.1 outline\nof every fruit", "Outline checks\n(size, fill, shape,\nborder, overlap)",
             "Classifiers:\nvalid, ripe, dry", "Colour of fruits\nand photo; filter,\nreliability")
  fill <- c("#EEF2F6", rep("#DDE7F2", 4), "#FBE8CF", "#FBE8CF", "#DCEFE4")
  xs <- seq(6.2, 93.8, length.out = length(steps))
  for (j in seq_along(steps)) {
    rect(xs[j] - 5.7, 15, xs[j] + 5.7, 39, col = fill[j], border = NA)
    text(xs[j], 27, steps[j], cex = 0.62, col = ink)
    if (j < length(steps)) arrows(xs[j] + 5.8, 27, xs[j + 1] - 5.8, 27, length = 0.05, col = ink2, lwd = 1.2)
  }
  rect(xs[2] - 5.7, 11.5, xs[7] + 5.7, 12.5, col = c_mid, border = NA)
  text(mean(xs[2:7]), 6.5, "Python back end (fr_process), run as a separate process", cex = 0.66, col = c_mid)
  rect(xs[8] - 5.7, 11.5, xs[8] + 5.7, 12.5, col = c_keep, border = NA)
  text(xs[8], 6.5, "R", cex = 0.66, col = c_keep)
  plab("g", line = -0.6, adj = 0)
})

# ---------------------------------------------------------------- Figure 2: validation data
fig("fig2_data.png", 7.2, 6.5, {
  par(oma = c(0.4, 0, 0, 0))
  layout(matrix(c(rep(1, 7), 2:15), 3, byrow = TRUE), heights = c(2.35, 1, 1.08))
  par(mar = c(1.6, 0.2, 1.8, 0.2))
  plot.new()
  frame_lat <- seq(-58, 90, 0.5)
  fl <- eqearth(rep(c(-180, 180), each = length(frame_lat)), c(frame_lat, rev(frame_lat)))
  bot <- eqearth(c(-180, 180), c(-58, -58))
  plot.window(range(fl$x), c(bot$y[1], max(fl$y)), asp = 1)
  polygon(fl$x, fl$y, col = "white", border = NA)
  for (lt in seq(-30, 60, 30)) { g <- eqearth(seq(-180, 180, 2), lt); lines(g$x, g$y, col = grid_col, lwd = 0.5) }
  for (ln in seq(-150, 150, 30)) { g <- eqearth(ln, frame_lat); lines(g$x, g$y, col = grid_col, lwd = 0.5) }
  if (requireNamespace("maps", quietly = TRUE)) {
    w <- maps::map("world", wrap = c(-180, 180), fill = TRUE, plot = FALSE)
    p <- eqearth(w$x, pmax(w$y, -58))
    polygon(p$x, p$y, col = "#E3E6EA", border = "#C9CED6", lwd = 0.3)
  }
  lines(fl$x, fl$y, col = ink2, lwd = 0.6)
  pv <- v[v$position == "first", ]
  pp <- eqearth(pv$longitude, pv$latitude)
  points(pp$x, pp$y, pch = 21, cex = 0.55, lwd = 0.25, col = "white", bg = grDevices::adjustcolor(c_mid, 0.75))
  pb <- eqearth(b$photos$longitude, b$photos$latitude)
  points(pb$x, pb$y, pch = 24, cex = 0.65, lwd = 0.3, col = "white", bg = grDevices::adjustcolor(c_hi, 0.95))
  legend("bottom", c(sprintf("Repeatability pairs: %s observations of %d species", format(nrow(pv), big.mark = ","),
                             length(unique(v$species))),
                     sprintf("Benchmark: %d photographs of %d species", nrow(b$photos), length(unique(b$photos$species)))),
         pch = c(21, 24), pt.bg = c(c_mid, c_hi), col = "white", pt.cex = 1.3, bty = "n", cex = 0.78, text.col = ink,
         horiz = TRUE, inset = c(0, -0.07), xpd = NA)
  ptitle("Validation photographs (Equal Earth projection)")
  plab("a", adj = 0)
  par(mar = c(1.45, 0.25, 1.35, 0.25))
  ord <- order(match(gal$cls, c("F", "Y", "P", "C", "N")), match(gal$stage, c("R", "U", "D")))
  g <- gal[ord, ]
  cls_lab <- c(FR = "Single fleshy fruit,\nripe", FU = "Single fleshy fruit,\nunripe", FD = "Single fleshy fruit,\ndamaged",
               Y = "Single dry fruit", P = "Partial or\nwrong outline", C = "Several fruits", N = "Not a fruit")
  key <- ifelse(g$cls == "F", paste0("F", g$stage), g$cls)
  tiles <- c(seq(1, 13, 2), seq(2, 14, 2))
  for (j in seq_len(14)) {
    k <- tiles[j]
    im <- fr_read_image(file.path(gal_dir, paste0(g$file[k], ".jpg")))
    mm <- fr_read_mask(file.path(gal_dir, paste0(g$file[k], ".png")))
    cl <- if (key[k] == "FR") c_keep else if (key[k] %in% c("FU", "FD", "Y")) c_hi else c_rej
    dd <- draw_img(im, border = cl, lwd = 2.4, square = TRUE)
    outline(mm, "white", 1.1, halo = TRUE, dx = dd["x0"], dy = dd["y0"])
    if (j <= 7) mtext(cls_lab[key[k]], side = 3, line = 0.1, cex = 0.5, col = ink, font = 2)
    lab_p <- if (is.na(g$p_ripe[k])) sprintf("valid %.2f", g$p_valid[k]) else
      sprintf("valid %.2f, ripe %.2f", g$p_valid[k], g$p_ripe[k])
    mtext(g$species[k], side = 1, line = 0.05, cex = 0.46, font = 3, col = ink)
    mtext(lab_p, side = 1, line = 0.6, cex = 0.42, col = ink2)
    if (j == 1) plab("b", line = 0.6, adj = 0)
  }
})

# ---------------------------------------------------------------- Figure 3: outline checks and ratings
ol <- b$outlines
fig("fig3_outlines.png", 7.2, 5.6, {
  layout(matrix(1:4, 2, byrow = TRUE), widths = c(1, 1.1))
  par(mar = c(3.4, 7.4, 2, 1.4))
  ch <- c(passed = sum(b$photos$n_passed), small = sum(b$photos$n_small), shape = sum(b$photos$n_shape),
          fill = sum(b$photos$n_fill), border = sum(b$photos$n_border), size = sum(b$photos$n_size),
          overlap = sum(b$photos$n_overlap))
  lab_ch <- c(passed = "passed all checks", small = "< 800 px", shape = "solidity < 0.80", fill = "fills < 35% of box",
              border = "cut by crop border", size = "> 6 x median fruit", overlap = "overlaps another fruit")
  o <- rev(seq_along(ch))
  bp <- barplot(ch[o], horiz = TRUE, names.arg = lab_ch[names(ch)[o]], col = ifelse(names(ch)[o] == "passed", c_keep, c_rej),
                border = NA, xlim = c(0, max(ch) * 1.18), cex.names = 0.8, xlab = "Outlines")
  text(ch[o], bp, format(ch[o], big.mark = ","), pos = 4, cex = 0.72, col = ink, xpd = NA)
  ptitle(sprintf("%s outlines in %d photographs", format(sum(b$photos$n_outlined), big.mark = ","), nrow(b$photos)))
  plab("a", adj = -0.55)
  par(mar = c(3.4, 7.4, 2, 1.4))
  cls <- c(F = "single fleshy fruit", Y = "single dry fruit", P = "partial or wrong", C = "several fruits", N = "not a fruit")
  nc <- table(factor(ol$cls, names(cls)))
  o <- rev(seq_along(nc))
  bp <- barplot(as.numeric(nc)[o], horiz = TRUE, names.arg = cls[o], border = NA, xlim = c(0, max(nc) * 1.3),
                col = c(F = c_keep, Y = c_hi, P = c_rej, C = c_rej, N = c_rej)[names(cls)[o]], cex.names = 0.8,
                xlab = "Rated outlines")
  text(as.numeric(nc)[o], bp, sprintf("%d (%.1f%%)", as.numeric(nc)[o], 100 * as.numeric(nc)[o] / sum(nc)), pos = 4,
       cex = 0.72, col = ink, xpd = NA)
  st <- table(factor(ol$stage[ol$cls %in% c("F", "Y")], c("R", "U", "D")), useNA = "no")
  nund <- sum(ol$cls %in% c("F", "Y") & is.na(ol$stage))
  mtext(sprintf("Single fruits: %d ripe, %d unripe, %d damaged, %d undecided", st["R"], st["U"], st["D"], nund),
        side = 1, line = 3.5, cex = 0.62, col = ink2)
  ptitle(sprintf("Ratings of the %s outlines that passed", format(nrow(ol), big.mark = ",")))
  plab("b", adj = -0.55)
  par(mar = c(3.6, 3.6, 2, 0.8))
  kk <- factor(ol$cls, names(cls))
  set.seed(1)
  plot(1, 1, type = "n", xlim = c(0.5, 5.5), ylim = range(ol$diam_px), log = "y", xaxt = "n", xlab = "",
       ylab = "Equivalent diameter (px)")
  gridlines(h = c(32, 50, 100, 200, 500))
  abline(h = 2 * sqrt(800 / pi), lty = 2, col = ink2)
  for (j in seq_along(cls)) {
    y <- ol$diam_px[kk == names(cls)[j]]
    points(j + stats::runif(length(y), -0.28, 0.28), y, pch = 16, cex = 0.35,
           col = grDevices::adjustcolor(c(c_keep, c_hi, c_rej, c_rej, c_rej)[j], 0.45))
    q <- stats::quantile(y, c(0.25, 0.5, 0.75))
    rect(j - 0.3, q[1], j + 0.3, q[3], border = ink, lwd = 1)
    segments(j - 0.3, q[2], j + 0.3, q[2], lwd = 2, col = ink)
  }
  axis(1, at = 1:5, labels = c("fleshy", "dry", "partial", "several", "not fruit"), cex.axis = 0.78)
  ptitle("Size of rated outlines (box: quartiles)")
  plab("c", adj = -0.15)
  par(mar = c(3.6, 3.6, 2, 0.8))
  br <- c(30, 50, 80, 130, 200, Inf)
  bin <- cut(ol$diam_px, br, right = FALSE)
  share <- tapply(ol$cls %in% c("F", "Y"), bin, mean)
  nb <- table(bin)
  bp <- barplot(share, ylim = c(0, 1), col = c_mid, border = NA, names.arg = c("32-49", "50-79", "80-129", "130-199", "200+"),
                ylab = "Share rated single fruit", xlab = "Equivalent diameter (px)", cex.names = 0.78)
  gridlines(h = seq(0, 1, 0.2))
  barplot(share, add = TRUE, col = c_mid, border = NA, axes = FALSE, axisnames = FALSE)
  text(bp, share, sprintf("n = %d", nb), pos = 3, cex = 0.66, col = ink, xpd = NA)
  ptitle("Single fruits among outlines by size")
  plab("d", adj = -0.15)
})

# ---------------------------------------------------------------- Figure 4: fruit classifiers
fig("fig4_classifiers.png", 7.2, 6.2, {
  layout(matrix(1:4, 2, byrow = TRUE))
  par(mar = c(3.6, 3.6, 2, 0.8))
  single <- ol$cls %in% c("F", "Y")
  rv <- roc(ol$p_valid, single)
  rp_rows <- single & !is.na(ol$stage) & !is.na(ol$p_ripe)
  rr <- roc(ol$p_ripe[rp_rows], ol$stage[rp_rows] == "R")
  rd_rows <- single & !is.na(ol$p_dry)
  rd <- roc(ol$p_dry[rd_rows], ol$cls[rd_rows] == "Y")
  plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "False-positive rate", ylab = "True-positive rate")
  gridlines(h = seq(0, 1, 0.2), v = seq(0, 1, 0.2))
  abline(0, 1, lty = 3, col = ink2)
  lines(rv$fp, rv$tp, col = head_col["valid"], lwd = 2)
  lines(rr$fp, rr$tp, col = head_col["ripe"], lwd = 2)
  lines(rd$fp, rd$tp, col = head_col["dry"], lwd = 2, lty = 2)
  legend("bottomright", c(sprintf("valid (%s outlines), AUC %.2f", format(sum(!is.na(ol$p_valid)), big.mark = ","), rv$auc),
                          sprintf("ripe (%d fruits), AUC %.2f", sum(rp_rows), rr$auc),
                          sprintf("dry (%d fruits), AUC %.2f", sum(rd_rows), rd$auc)),
         col = head_col, lwd = 2, lty = c(1, 1, 2), bty = "n", cex = 0.72, text.col = ink)
  ptitle("Out-of-fold ROC curves (folds by species)")
  plab("a", adj = -0.15)
  ru <- b$rules
  plot(0, type = "n", xlim = range(ru$threshold), ylim = c(0.4, 1), xlab = "Probability threshold",
       ylab = "Precision or recall")
  gridlines(h = seq(0.4, 1, 0.1), v = ru$threshold)
  abline(v = 0.5, lty = 2, col = ink2)
  lines(ru$threshold, ru$valid_precision, col = head_col["valid"], lwd = 2); points(ru$threshold, ru$valid_precision, pch = 21, bg = head_col["valid"], col = "white", cex = 1.2)
  lines(ru$threshold, ru$valid_recall, col = head_col["valid"], lwd = 2, lty = 2); points(ru$threshold, ru$valid_recall, pch = 21, bg = "white", col = head_col["valid"], cex = 1.1)
  lines(ru$threshold, ru$ripe_precision, col = head_col["ripe"], lwd = 2); points(ru$threshold, ru$ripe_precision, pch = 24, bg = head_col["ripe"], col = "white", cex = 1.1)
  lines(ru$threshold, ru$ripe_recall, col = head_col["ripe"], lwd = 2, lty = 2); points(ru$threshold, ru$ripe_recall, pch = 24, bg = "white", col = head_col["ripe"], cex = 1)
  legend("bottomleft", c("valid: precision", "valid: recall", "ripe: precision", "ripe: recall"),
         col = head_col[c("valid", "valid", "ripe", "ripe")], lty = c(1, 2, 1, 2), pch = c(21, 21, 24, 24),
         pt.bg = c(head_col["valid"], "white", head_col["ripe"], "white"), lwd = 1.6, bty = "n", cex = 0.7,
         text.col = ink)
  ptitle("Decision thresholds (same threshold for both)")
  plab("b", adj = -0.15)
  par(mar = c(3.6, 4.2, 2, 0.8))
  for (who in c("ripe", "unripe or damaged")) {
    rc <- b$ripe_colour[b$ripe_colour$rated == who & b$ripe_colour$fruits >= 5, ]
    rc <- rc[order(rc$share_classified_ripe), ]
    bp <- barplot(rc$share_classified_ripe, names.arg = rc$name, col = name_col[rc$name], border = "grey55",
                  ylim = c(0, 1.15), ylab = "Share classified ripe", cex.names = 0.75, las = 2, axes = FALSE)
    axis(2, at = seq(0, 1, 0.2))
    abline(h = if (who == "ripe") 1 else 0, col = ink2, lwd = 0.8)
    text(bp, rc$share_classified_ripe, rc$fruits, pos = 3, cex = 0.62, col = ink, xpd = NA)
    ptitle(if (who == "ripe") "Fruits rated ripe, by colour name" else "Fruits rated unripe or damaged, by colour name")
    plab(if (who == "ripe") "c" else "d", adj = -0.18)
  }
})

# ---------------------------------------------------------------- Figure 5: trait reliability
num <- ct[ct$level == "photo" & ct$type %in% c("numeric", "circular"), ]
fig("fig5_reliability.png", 7.2, 6.8, {
  layout(matrix(c(1, 2, 1, 3, 4, 4), 3, byrow = TRUE), heights = c(1, 1, 0.95), widths = c(1.15, 1))
  par(mar = c(3.6, 6.6, 2, 1))
  n <- num[order(num$icc_measurable), ]
  yy <- seq_len(nrow(n))
  plot(0, type = "n", xlim = c(min(0, n$icc_lower, na.rm = TRUE), 1), ylim = c(0.5, nrow(n) + 0.5), yaxt = "n",
       xlab = "Photo-to-photo repeatability (ICC)", ylab = "")
  gridlines(v = seq(0, 1, 0.2))
  abline(v = c(0.4, 0.6), lty = c(3, 2), col = ink2)
  segments(n$icc_lower, yy, n$icc_upper, yy, col = verdict_col[as.character(n$verdict)], lwd = 1.6)
  points(n$icc_measurable, yy, pch = 21, bg = verdict_col[as.character(n$verdict)], col = "white", cex = 1.3)
  points(n$icc_all, yy, pch = 4, cex = 0.7, col = ink2)
  axis(2, at = yy, labels = n$trait, tick = FALSE, cex.axis = 0.78, col.axis = ink)
  vv <- names(verdict_col)[names(verdict_col) %in% as.character(n$verdict)]
  legend("bottomright", c(sprintf("%s (%d)", vv, table(factor(n$verdict, vv))), "all pairs"),
         pch = c(rep(21, length(vv)), 4), pt.bg = c(verdict_col[vv], NA), col = c(rep("white", length(vv)), ink2),
         pt.cex = 1.3, bty = "n", cex = 0.7, text.col = ink)
  ptitle(sprintf("Repeatability: %d pairs measurable in both (95%% CI)", n$n_pairs[1]))
  plab("a", adj = -0.27)
  m <- v[v$both_measurable, ]
  f1v <- m[m$position == "first", ]
  s1v <- m[m$position == "second", ][match(f1v$occurrenceKey, m$occurrenceKey[m$position == "second"]), ]
  par(mar = c(3.4, 3.6, 2, 0.8))
  for (q in list(c("m_L", "L*", "b"), c("m_C", "C*", "c"))) {
    x <- f1v[[q[1]]]; y <- s1v[[q[1]]]
    lim <- range(c(x, y), na.rm = TRUE)
    plot(x, y, type = "n", xlim = lim, ylim = lim, xlab = paste(q[2], "of the first photograph"),
         ylab = paste(q[2], "of the second photograph"))
    gridlines(h = pretty(lim), v = pretty(lim))
    abline(0, 1, lty = 2, col = ink2)
    points(x, y, pch = 21, cex = 0.75, lwd = 0.3, col = "grey35",
           bg = lab_hex((f1v$m_L + s1v$m_L) / 2, (f1v$m_a + s1v$m_a) / 2, (f1v$m_b + s1v$m_b) / 2))
    icc <- ct$icc_measurable[ct$trait == q[1]]
    legend("topleft", sprintf("ICC = %.2f", icc), bty = "n", cex = 0.8, text.col = ink)
    ptitle(paste("Repeatability of", q[2]))
    plab(q[3], adj = -0.2)
  }
  iv <- b$invariance
  rownames(iv) <- iv$trait
  cols <- c("white_balance", "exposure", "jpeg", "blur", "rotate", "perspective", "D8", "D16", "D32", "D64")
  M <- as.matrix(iv[c("L", "a", "b", "C", "hue_deg"), cols])
  par(mar = c(3.4, 4.6, 2.4, 1))
  pal <- grDevices::colorRampPalette(c("#B5443B", "#F2D6A2", "#DCEFE4", "#2E7D5B"))(100)
  zr <- c(0.9, 1)
  plot(0, type = "n", xlim = c(0.5, ncol(M) + 0.5), ylim = c(0.5, nrow(M) + 0.5), axes = FALSE, xlab = "", ylab = "")
  for (i in seq_len(nrow(M))) for (j in seq_len(ncol(M))) {
    z <- (min(max(M[i, j], zr[1]), zr[2]) - zr[1]) / diff(zr)
    fc <- pal[1 + round(z * 99)]
    rect(j - 0.5, nrow(M) - i + 0.5, j + 0.5, nrow(M) - i + 1.5, col = fc, border = "white", lwd = 1.5)
    lum <- sum(c(0.299, 0.587, 0.114) * grDevices::col2rgb(fc)[, 1]) / 255
    text(j, nrow(M) - i + 1, sprintf("%.3f", M[i, j]), cex = 0.66, col = if (lum < 0.55) "white" else ink)
  }
  axis(2, at = nrow(M):1, labels = c("L*", "a*", "b*", "C*", "hue"), tick = FALSE, cex.axis = 0.8, col.axis = ink)
  axis(1, at = seq_len(ncol(M)), labels = c("white\nbalance", "exposure", "JPEG", "blur", "rotation", "perspective",
                                            "8 px", "16 px", "32 px", "64 px"), tick = FALSE, cex.axis = 0.68,
       padj = 0.5, col.axis = ink)
  mtext("Camera changes", side = 3, at = 3.5, line = 0.1, cex = 0.62, col = ink2)
  mtext("Median fruit diameter after reduction", side = 3, at = 8.5, line = 0.1, cex = 0.62, col = ink2)
  abline(v = 6.5, col = ink2, lwd = 1)
  n0 <- b$invariance_n[b$invariance_n$condition == "white_balance", ]
  ptitle(sprintf("Invariance: ICC with the original crop (%d fruits in %d crops)", n0$fruits, n0$crops), line = 1)
  plab("d", adj = -0.06, line = 1)
})

# ---------------------------------------------------------------- Figure 6: photographs against reflectance spectra
fig("fig6_spectra.png", 7.2, 6.2, {
  layout(matrix(1:4, 2, byrow = TRUE))
  par(mar = c(3.6, 3.6, 2, 0.8))
  bgc <- lab_hex(rf$L, rf$a, rf$b)
  qq <- list(list("C", "spectral_saturation", "C* of the photographs", "Spectral saturation", "a"),
             list("L", "spectral_lightness", "L* of the photographs", "Spectral lightness", "b"),
             list("hue_shift", "spectral_hue_nm", "Hue angle of the photographs (degrees)", "Spectral hue (nm)", "c"))
  rf$hue_shift <- (rf$hue_deg + 30) %% 360 - 30
  for (q in qq) {
    x <- rf[[q[[1]]]]; y <- rf[[q[[2]]]]
    plot(x, y, type = "n", xlab = q[[3]], ylab = q[[4]])
    gridlines(h = pretty(y), v = pretty(x))
    points(x, y, pch = 21, bg = bgc, col = "grey35", cex = 1.1, lwd = 0.4)
    ct_ <- stats::cor.test(x, y, method = "spearman", exact = FALSE)
    ptxt <- if (ct_$p.value < 0.001) "< 0.001" else sprintf("= %.2f", ct_$p.value)
    legend(if (q[[5]] == "c") "right" else "topleft",
           as.expression(bquote(rho == .(sprintf("%.2f", ct_$estimate)) * ", p " * .(ptxt))),
           bty = "n", cex = 0.85, text.col = ink, bg = "white")
    ptitle(sprintf("%d species", sum(stats::complete.cases(x, y))))
    plab(q[[5]], adj = -0.15)
  }
  par(mar = c(4.6, 4.4, 2, 0.8))
  map_nm <- c(pink = "red", purple = "black")
  pn <- ifelse(rf$photo_name %in% names(map_nm), map_nm[rf$photo_name], rf$photo_name)
  lv <- c("red", "black", "blue", "orange", "yellow", "brown", "white", "green", "grey")
  tb <- table(factor(pn, lv), factor(rf$spectrum_colour, lv))
  tb <- tb[rowSums(tb) > 0, colSums(tb) > 0, drop = FALSE]
  plot(0, type = "n", xlim = c(0.5, ncol(tb) + 0.5), ylim = c(0.5, nrow(tb) + 0.5), axes = FALSE,
       xlab = "", ylab = "")
  mtext("Colour of the spectrum entry", side = 1, line = 3.6, cex = 0.8, col = ink)
  for (i in seq_len(nrow(tb))) for (j in seq_len(ncol(tb))) {
    z <- tb[i, j]
    rect(j - 0.5, nrow(tb) - i + 0.5, j + 0.5, nrow(tb) - i + 1.5, border = "white", lwd = 1.5,
         col = if (z == 0) "#F4F5F7" else grDevices::adjustcolor(if (rownames(tb)[i] == colnames(tb)[j]) c_keep else c_mid,
                                                                min(1, 0.15 + z / 20)))
    if (z > 0) text(j, nrow(tb) - i + 1, z, cex = 0.72, col = ink)
  }
  axis(1, at = seq_len(ncol(tb)), labels = colnames(tb), tick = FALSE, cex.axis = 0.72, las = 2)
  axis(2, at = nrow(tb):1, labels = rownames(tb), tick = FALSE, cex.axis = 0.72)
  mtext("Colour name of the photographs", side = 2, line = 3.2, cex = 0.72, las = 0, col = ink)
  ptitle(sprintf("Same colour for %.1f%% of species", 100 * mean(pn == rf$spectrum_colour)))
  plab("d", adj = -0.2)
})

# ---------------------------------------------------------------- Figure 7: R against the back end, augmentation
fig("fig7_consistency.png", 7.2, 6.4, {
  layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), heights = c(1, 1.05))
  a <- b$r_python
  de <- fr_delta_e(cbind(a$L_r, a$a_r, a$b_r), cbind(a$L_py, a$a_py, a$b_py))
  par(mar = c(3.6, 3.6, 2, 0.8))
  h <- graphics::hist(de, breaks = seq(0, ceiling(max(de) * 10) / 10, 0.05), plot = FALSE)
  plot(h, col = c_mid, border = "white", main = "", xlab = "CIEDE2000 difference, R vs back end", ylab = "Fruits")
  abline(v = stats::quantile(de, c(0.5, 0.99)), lty = c(1, 2), col = ink)
  legend("topright", c(sprintf("median %.2f", stats::median(de)), sprintf("99th percentile %.2f", stats::quantile(de, 0.99)),
                       sprintf("same colour name: %.1f%%", 100 * mean(a$name_r == a$name_py))),
         lty = c(1, 2, NA), col = ink, bty = "n", cex = 0.72, text.col = ink)
  ptitle(sprintf("%s fruits in %d benchmark crops", format(nrow(a), big.mark = ","), length(unique(a$photo_id))))
  plab("a", adj = -0.15)
  par(mar = c(3.6, 3.6, 2, 0.8))
  plot(a$L_py, a$L_r, pch = 16, cex = 0.4, col = grDevices::adjustcolor(ink, 0.35), xlab = "L* (back end)",
       ylab = "L* (R)")
  abline(0, 1, lty = 2, col = c_rej)
  ptitle("Lightness of each fruit")
  plab("b", adj = -0.15)
  ag <- b$augmentation
  par(mar = c(4.8, 3.8, 2.2, 0.8))
  tys <- c("rotate90", "flip", "rotate", "brightness", "contrast", "saturation", "hue", "white_balance", "exposure",
           "jpeg", "blur")
  trs <- c("L", "a", "b", "C", "hue_deg")
  tcol <- c(L = "#262626", a = "#D7301F", b = "#E3B505", C = c_mid, hue_deg = "#8E5DB5")
  med <- tapply(ag$rel_change, list(factor(ag$type, tys), factor(ag$trait, trs)), stats::median, na.rm = TRUE)
  q3 <- tapply(ag$rel_change, list(factor(ag$type, tys), factor(ag$trait, trs)), stats::quantile, 0.75, na.rm = TRUE)
  ymax <- max(0.3, unlist(q3), na.rm = TRUE) * 1.1
  plot(0, type = "n", xlim = c(0.5, length(tys) + 0.5), ylim = c(0, ymax), xaxt = "n", xlab = "",
       ylab = "Change / between-photo SD")
  gridlines(h = pretty(c(0, ymax)))
  abline(h = 0.25, lty = 2, col = c_rej)
  for (k in seq_along(trs)) {
    xx <- seq_along(tys) + (k - 3) * 0.14
    segments(xx, med[, k], xx, q3[, k], col = tcol[k], lwd = 1.2)
    points(xx, med[, k], pch = 21, bg = tcol[k], col = "white", cex = 1.15)
  }
  axis(1, at = seq_along(tys), labels = c("quarter\nturn", "mirror", "rotation", "brightness", "contrast",
                                          "saturation", "hue", "white\nbalance", "exposure", "JPEG", "blur"),
       tick = FALSE, cex.axis = 0.72, padj = 0.5)
  legend("topleft", c("L*", "a*", "b*", "C*", "hue"), pch = 21, pt.bg = tcol, col = "white", pt.cex = 1.3, bty = "n",
         cex = 0.72, horiz = TRUE, text.col = ink)
  ptitle(sprintf("Augmentation check: median and upper quartile over %d crops", length(unique(ag$photo_id))))
  plab("c", adj = -0.06)
})

# ---------------------------------------------------------------- Figure 8: worked example
fig("fig8_example.png", 7.2, 4.6, {
  par(mfrow = c(2, 4), mar = c(2.6, 0.3, 1.4, 0.3), oma = c(1.6, 0, 0, 0))
  for (i in seq_len(nrow(ex))) {
    im <- fr_read_image(ex$crop[i]); lb <- fr_read_labels(ex$labels[i])
    fi <- fru[fru$photo_id == ex$photo_id[i] & fru$ok %in% TRUE, ]
    d <- draw_img(im, square = TRUE)
    for (l in fi$label) {
      st <- fi[fi$label == l, ]
      cl <- if (st$measurable %in% TRUE) c_keep else if (st$valid %in% TRUE) c_hi else c_rej
      outline(lb == l, cl, 1.6, halo = TRUE, dx = d["x0"], dy = d["y0"])
    }
    mtext(ex$species[i], side = 3, line = 0.2, cex = 0.62, font = 3, col = ink)
    ri <- res[i, ]
    s <- d["w"] + 2 * d["x0"]
    if (isTRUE(ri$measurable)) {
      rect(0.02 * s, -0.11 * s, 0.13 * s, -0.02 * s, col = lab_hex(ri$m_L, ri$m_a, ri$m_b), border = "grey40", xpd = NA)
      text(0.16 * s, -0.065 * s, sprintf("%s: L* %.0f, C* %.0f, %d fruit%s", ri$m_name, ri$m_L, ri$m_C, ri$m_n,
                                         if (ri$m_n == 1) "" else "s"), adj = 0, cex = 0.7, col = ink, xpd = NA)
    } else {
      text(0.02 * s, -0.065 * s, sprintf("not measurable (%d valid, unripe)", sum(fi$valid %in% TRUE)), adj = 0,
           cex = 0.7, col = ink2, xpd = NA)
    }
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot.new()
  legend("bottom", c("measurable (valid and ripe)", "valid, not ripe", "not a single fruit with a correct outline"),
         col = c(c_keep, c_hi, c_rej), lwd = 2, horiz = TRUE, bty = "n", cex = 0.78, text.col = ink)
})

# ---------------------------------------------------------------- Figure S1: feature sets of the classifiers
fs <- b$feature_sets
fig("figS1_featuresets.png", 7.2, 3.6, {
  par(mfrow = c(1, 3), mar = c(3.6, 8.6, 2, 0.8))
  for (h in c("valid", "ripe", "dry")) {
    d <- fs[fs$head == h, ]
    d <- d[order(d$auc), ]
    yy <- seq_len(nrow(d))
    used <- d$features == b$heads$feature_set[b$heads$head == h]
    plot(0, type = "n", xlim = range(c(0.5, 1, d$auc - d$auc_sd)), ylim = c(0.5, nrow(d) + 0.5), yaxt = "n",
         xlab = "Cross-validated AUC", ylab = "")
    gridlines(v = seq(0.5, 1, 0.1))
    abline(v = 0.8, lty = 2, col = ink2)
    segments(d$auc - d$auc_sd, yy, d$auc + d$auc_sd, yy, col = head_col[h], lwd = 1.4)
    points(d$auc, yy, pch = 21, bg = ifelse(used, head_col[h], "white"), col = head_col[h], cex = 1.2)
    axis(2, at = yy, labels = gsub("_", " ", d$features), tick = FALSE, cex.axis = 0.72, col.axis = ink)
    ptitle(sprintf("%s classifier (%s fruits)", h, format(d$n[1], big.mark = ",")))
    plab(c(valid = "a", ripe = "b", dry = "c")[h], adj = -0.85)
  }
})
message("Figures written to ", normalizePath(out))
