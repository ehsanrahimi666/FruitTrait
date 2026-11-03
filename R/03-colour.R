#' Convert sRGB colours to CIELAB
#'
#' Converts sRGB values (0-255) to CIELAB (D65) through linear RGB, with the
#' constants of the back end ('OpenCV'), so that colours computed in R and in
#' the back end agree.
#'
#' @param img An RGB array (values 0-255), a file path, or a three-column
#'   matrix of RGB colours.
#' @return For a matrix, a matrix with columns `L`, `a` and `b`; otherwise an
#'   array of the same size as `img` with L*, a* and b* in the three layers.
#' @examples
#' fr_rgb_to_lab(rbind(cherry = c(150, 20, 35), blueberry = c(60, 70, 110)))
#' @export
fr_rgb_to_lab <- function(img) {
  if (is.matrix(img) && ncol(img) == 3L) {
    lab <- .fr_lab(.fr_lin(img[, 1]), .fr_lin(img[, 2]), .fr_lin(img[, 3]))
    out <- cbind(L = lab$L, a = lab$a, b = lab$b)
    rownames(out) <- rownames(img)
    return(out)
  }
  img <- .fr_as_image(img)
  lab <- .fr_lab(.fr_lin(img[, , 1]), .fr_lin(img[, , 2]), .fr_lin(img[, , 3]))
  array(c(lab$L, lab$a, lab$b), dim(img))
}

#' Colour of outlined fruits
#'
#' Computes, in R, the colour of every fruit of a label map as the back end
#' does: the CIELAB mean (D65) of the pixels of the fruit without its brightest
#' 2% (mostly specular highlights), its chroma and hue angle and its colour
#' name, optionally after a white balance of the crop. Use it to measure fruits
#' outlined by hand or corrected outlines.
#'
#' @param img Crop: RGB array (values 0-255) or file path.
#' @param labels Label map (integer matrix, 0 = background) or PNG file path,
#'   for example `fruits/<photo_id>.png` of the back end; a logical mask counts
#'   as one fruit.
#' @param white_balance `"none"` or a method of [fr_white_balance()], applied
#'   to the whole crop before the colour is measured.
#' @param min_px Fruits with fewer pixels are skipped.
#' @return A data frame with one row per fruit: `label`, `px`, `L`, `a`, `b`,
#'   `C`, `hue_deg` and `name`.
#' @seealso [fr_fruits()] for the colours computed by the back end.
#' @examples
#' ex <- fr_example_photos()
#' fr_fruit_colour(ex$crop[1], ex$labels[1])
#' @export
fr_fruit_colour <- function(img, labels, white_balance = c("none", "gray_world", "white_patch",
                                                           "shades_of_gray", "gray_pixels"),
                            min_px = 10) {
  white_balance <- match.arg(white_balance)
  img <- .fr_as_image(img)
  lab_map <- .fr_as_labels(labels)
  if (!identical(dim(lab_map), dim(img)[1:2])) stop("`labels` and `img` differ in size.", call. = FALSE)
  if (white_balance != "none") img <- fr_white_balance(img, white_balance)
  lab <- fr_rgb_to_lab(img)
  L_all <- lab[, , 1]
  ids <- sort(setdiff(unique(as.vector(lab_map)), 0L))
  rows <- lapply(ids, function(k) {
    m <- lab_map == k
    n <- sum(m)
    if (n < min_px) return(NULL)
    L <- L_all[m]
    hi <- stats::quantile(L, 0.98, names = FALSE, type = 7)
    use <- if (sum(L <= hi) >= 10) m & L_all <= hi else m
    v <- c(mean(lab[, , 1][use]), mean(lab[, , 2][use]), mean(lab[, , 3][use]))
    data.frame(label = k, px = n, L = v[1], a = v[2], b = v[3], C = sqrt(v[2]^2 + v[3]^2),
               hue_deg = (atan2(v[3], v[2]) * 180 / pi) %% 360, name = fr_colour_name(v[1], v[2], v[3]))
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(label = integer(), px = integer(), L = numeric(), a = numeric(), b = numeric(),
                      C = numeric(), hue_deg = numeric(), name = character()))
  }
  do.call(rbind, rows)
}

#' Colour difference CIEDE2000
#'
#' @param lab1,lab2 CIELAB colours: vectors of length 3 or three-column
#'   matrices (recycled row-wise).
#' @return The CIEDE2000 colour differences.
#' @examples
#' fr_delta_e(c(30, 40, 20), c(32, 38, 25))
#' @export
fr_delta_e <- function(lab1, lab2) {
  .fr_de00(lab1, lab2)
}

#' Colour correction (white balance)
#'
#' Estimates the colour of the light from the photo and divides it out in
#' linear RGB, as in the back end: grey world (mean of all pixels), white patch
#' (99th percentile), shades of grey (Minkowski mean with `p = 6`) or grey
#' pixels (mean of near-neutral pixels: saturation below 0.2 and brightest
#' channel between 10% and 98%). The estimate is scaled to keep the overall
#' brightness. Grey world assumes that the scene is grey on average, which green
#' vegetation and coloured fruits violate; the back end reports grey-world
#' corrected fruit colours (`wb_L`, `wb_a`, `wb_b`) for comparison with the
#' uncorrected colours, not as a replacement.
#'
#' @param img RGB array (values 0-255) or file path.
#' @param method `"gray_world"`, `"white_patch"`, `"shades_of_gray"` or
#'   `"gray_pixels"`.
#' @param mask Optional fruit mask; with `use = "background"`, the light is
#'   estimated from pixels outside the fruits.
#' @param use `"all"` pixels or `"background"` pixels.
#' @param p Exponent for shades of grey.
#' @return The corrected RGB array (values 0-255).
#' @examples
#' ex <- fr_example_photos()
#' wb <- fr_white_balance(fr_read_image(ex$crop[1]), "gray_world")
#' range(wb)
#' @export
fr_white_balance <- function(img, method = c("gray_world", "white_patch", "shades_of_gray", "gray_pixels"),
                             mask = NULL, use = c("all", "background"), p = 6) {
  method <- match.arg(method)
  use <- match.arg(use)
  img <- .fr_as_image(img)
  lin <- lapply(1:3, function(ch) .fr_lin(img[, , ch]))
  sel <- if (use == "background" && !is.null(mask)) !(.fr_as_labels(mask) > 0) else
    matrix(TRUE, dim(img)[1], dim(img)[2])
  if (method == "gray_pixels") {
    sel <- sel & .fr_neutral(img)
    if (sum(sel) < max(50, 0.01 * length(sel))) return(img)
    method <- "gray_world"
  }
  e <- vapply(lin, function(L) {
    v <- L[sel]
    switch(method, gray_world = mean(v), white_patch = stats::quantile(v, 0.99, names = FALSE),
           shades_of_gray = mean(v^p)^(1 / p))
  }, numeric(1))
  e <- pmax(e, 1e-6)
  e <- 3 * e / sum(e)
  out <- img
  for (ch in 1:3) out[, , ch] <- .fr_to_srgb8(lin[[ch]] / e[ch])
  out
}

#' Check the colour and exposure of photos
#'
#' Screens photos for a colour cast, clipped highlights and under-exposure. The
#' colour cast is estimated from near-neutral pixels outside the fruits (see
#' `"gray_pixels"` in [fr_white_balance()]) and expressed as log ratios of red
#' and blue to green and as the spread `cast` = log(largest / smallest
#' channel); it is `NA` when fewer than 1% of the pixels are neutral. The
#' thresholds are screening defaults, not validated limits.
#'
#' @param img RGB array, file path, or several file paths.
#' @param labels Optional fruit label map or mask (or paths, one per image).
#' @param max_cast Largest acceptable spread of the light estimate between
#'   channels, as a log ratio.
#' @param max_clip Largest acceptable share of clipped pixels (brightest
#'   channel of at least 250) in the fruits, or in the photo without a mask.
#' @param max_dark Largest acceptable share of very dark pixels (brightest
#'   channel of at most 5) in the fruits. Black fruits in shade can be flagged.
#' @return A data frame with `neutral`, `cast_rg`, `cast_bg`, `cast`, `clip`,
#'   `dark` and `issues`, with `photo_id` for file input.
#' @examples
#' ex <- fr_example_photos()
#' fr_colour_check(ex$crop, ex$labels)
#' @export
fr_colour_check <- function(img, labels = NULL, max_cast = 0.2, max_clip = 0.02, max_dark = 0.05) {
  if (is.character(img) && length(img) > 1L) {
    if (!is.null(labels) && length(labels) != length(img)) stop("Give one label map per image.", call. = FALSE)
    rows <- lapply(seq_along(img), function(i) fr_colour_check(img[i], if (is.null(labels)) NULL else labels[i],
                                                               max_cast, max_clip, max_dark))
    return(do.call(rbind, rows))
  }
  id <- if (is.character(img)) tools::file_path_sans_ext(basename(img)) else NULL
  img <- .fr_as_image(img)
  m <- if (is.null(labels)) NULL else .fr_as_labels(labels) > 0
  bg <- if (is.null(m)) matrix(TRUE, dim(img)[1], dim(img)[2]) else !m
  lin <- lapply(1:3, function(ch) .fr_lin(img[, , ch]))
  neu <- bg & .fr_neutral(img)
  ok <- sum(neu) >= max(50, 0.01 * length(neu))
  e <- if (ok) pmax(vapply(lin, function(L) mean(L[neu]), numeric(1)), 1e-6) else rep(NA_real_, 3)
  mx <- pmax(img[, , 1], img[, , 2], img[, , 3])
  sel <- if (is.null(m)) TRUE else m
  out <- data.frame(neutral = mean(neu), cast_rg = log(e[1] / e[2]), cast_bg = log(e[3] / e[2]))
  out$cast <- log(max(e) / min(e))
  out$clip <- mean(mx[sel] >= 250)
  out$dark <- mean(mx[sel] <= 5)
  issues <- c(if (!is.na(out$cast) && out$cast > max_cast) "colour cast",
              if (out$clip > max_clip) "clipped highlights", if (out$dark > max_dark) "under-exposed")
  out$issues <- if (length(issues)) paste(issues, collapse = "; ") else "ok"
  if (!is.null(id)) out <- cbind(photo_id = id, out, stringsAsFactors = FALSE)
  out
}

#' Human colour category of CIELAB colours
#'
#' Assigns one of 11 basic colour categories (white, grey, black, red, pink,
#' orange, yellow, brown, green, blue, purple) to CIELAB colours (D65) with the
#' fixed rule of the back end, which names the colour of every fruit (`name`
#' in [fr_fruits()]) and the mean colour of the ripe fruits of a photo
#' (`m_name`).
#'
#' Colours with chroma below `achromatic_C` are white (`L*` >= 60), grey (30-60)
#' or black (< 30). Chromatic colours are named by hue angle: 345-20 degrees
#' pink (`L*` >= 45) or red; 20-50 red (pink if `L*` >= 65 and chroma < 50;
#' brown if `L*` < 35 and chroma < 40); 50-80 orange (brown if `L*` < 45);
#' 80-115 yellow (brown if `L*` < 45); 115-200 green; 200-310 blue; 310-345
#' purple. Names describe colour as it appears in an sRGB photograph, not as
#' fruit-eating animals see it.
#'
#' @param L,a,b CIELAB coordinates (vectors of equal length), or a three-column
#'   matrix in `L`.
#' @param achromatic_C Chroma below which a colour counts as achromatic.
#' @return A character vector of colour names.
#' @examples
#' fr_colour_name(c(30, 15, 60), c(45, 5, -2), c(25, -12, 70))
#' @export
fr_colour_name <- function(L, a = NULL, b = NULL, achromatic_C = 15) {
  if (is.matrix(L) && ncol(L) == 3L) {
    a <- L[, 2]
    b <- L[, 3]
    L <- L[, 1]
  }
  if (is.null(a) || is.null(b) || length(a) != length(L) || length(b) != length(L)) {
    stop("Give `L`, `a` and `b` of equal length, or a three-column matrix.", call. = FALSE)
  }
  C <- sqrt(a^2 + b^2)
  h <- (atan2(b, a) * 180 / pi) %% 360
  out <- rep(NA_character_, length(L))
  ach <- !is.na(C) & C < achromatic_C
  out[ach] <- ifelse(L[ach] >= 60, "white", ifelse(L[ach] >= 30, "grey", "black"))
  ch <- !is.na(C) & !ach
  s1 <- ch & (h >= 345 | h < 20)
  out[s1] <- ifelse(L[s1] >= 45, "pink", "red")
  s2 <- ch & h >= 20 & h < 50
  out[s2] <- ifelse(L[s2] >= 65 & C[s2] < 50, "pink", ifelse(L[s2] < 35 & C[s2] < 40, "brown", "red"))
  s3 <- ch & h >= 50 & h < 80
  out[s3] <- ifelse(L[s3] < 45, "brown", "orange")
  s4 <- ch & h >= 80 & h < 115
  out[s4] <- ifelse(L[s4] < 45, "brown", "yellow")
  out[ch & h >= 115 & h < 200] <- "green"
  out[ch & h >= 200 & h < 310] <- "blue"
  out[ch & h >= 310 & h < 345] <- "purple"
  out
}

#' Convert reflectance spectra to CIELAB
#'
#' Computes the CIELAB colour of reflectance spectra under illuminant D65 for
#' the CIE 1931 2-degree standard observer, the colour space of the fruit
#' colours of the back end. Spectra are interpolated to 1 nm, held constant
#' beyond the measured range and integrated from 360 to 830 nm; the white point
#' is the perfect reflecting diffuser. Use it to compare photo colours with
#' spectrophotometric measurements of fruits.
#'
#' @param wavelength Wavelengths (nm), increasing.
#' @param reflectance Reflectance: a vector, or a matrix with one column per
#'   spectrum. Values are clipped to 0-1.2 after scaling.
#' @param percent Are reflectance values percentages? `NULL` (default) decides
#'   for all spectra together: percentages when any value exceeds 1.5.
#' @return A matrix with columns `L`, `a`, `b`, `C` (chroma) and `h` (hue angle,
#'   degrees), one row per spectrum.
#' @examples
#' wl <- 300:700
#' red_fruit <- 4 + 60 / (1 + exp(-(wl - 600) / 10))
#' lab <- fr_spectra_to_lab(wl, red_fruit)
#' lab
#' fr_colour_name(lab[, 1:3, drop = FALSE])
#' @export
fr_spectra_to_lab <- function(wavelength, reflectance, percent = NULL) {
  R <- as.matrix(reflectance)
  if (nrow(R) != length(wavelength)) R <- t(R)
  if (nrow(R) != length(wavelength)) stop("`reflectance` does not match `wavelength`.", call. = FALSE)
  if (is.unsorted(wavelength)) stop("`wavelength` must be increasing.", call. = FALSE)
  if (is.null(percent)) percent <- any(R > 1.5, na.rm = TRUE)
  if (percent) R <- R / 100
  cie <- .fr_cie
  E <- cie$D65
  k <- 100 / sum(E * cie$ybar)
  white <- k * c(sum(E * cie$xbar), sum(E * cie$ybar), sum(E * cie$zbar))
  out <- t(apply(R, 2, function(r) {
    ok <- is.finite(r)
    if (sum(ok) < 10) return(rep(NA_real_, 5))
    r <- pmin(pmax(r[ok], 0), 1.2)
    full <- stats::approx(wavelength[ok], r, xout = cie$wavelength, rule = 2)$y
    XYZ <- k * c(sum(E * full * cie$xbar), sum(E * full * cie$ybar), sum(E * full * cie$zbar))
    f <- function(t) ifelse(t > (6 / 29)^3, t^(1 / 3), t / (3 * (6 / 29)^2) + 4 / 29)
    fx <- f(XYZ[1] / white[1])
    fy <- f(XYZ[2] / white[2])
    fz <- f(XYZ[3] / white[3])
    L <- 116 * fy - 16
    a <- 500 * (fx - fy)
    b <- 200 * (fy - fz)
    c(L, a, b, sqrt(a^2 + b^2), (atan2(b, a) * 180 / pi) %% 360)
  }))
  colnames(out) <- c("L", "a", "b", "C", "h")
  rownames(out) <- colnames(R)
  out
}

# sRGB 0-255 -> linear RGB with the float32 precision of the back end.
.fr_lin_lut <- local({
  v <- (0:255) / 255
  readBin(writeBin(ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4), raw(), size = 4),
          "double", size = 4, n = 256L)
})

.fr_lin <- function(x) {
  i <- as.integer(round(x))
  if (anyNA(i) || any(i < 0L | i > 255L)) stop("RGB values must lie in 0-255.", call. = FALSE)
  out <- .fr_lin_lut[i + 1L]
  dim(out) <- dim(x)
  out
}

.fr_to_srgb8 <- function(lin) {
  x <- pmin(pmax(lin, 0), 1)
  s <- ifelse(x <= 0.0031308, 12.92 * x, 1.055 * x^(1 / 2.4) - 0.055)
  floor(s * 255 + 0.5)
}

# Linear RGB -> CIELAB with the constants of OpenCV (D65 white point Xn = 0.950456, Zn = 1.088754).
.fr_lab <- function(R, G, B) {
  X <- (0.412453 * R + 0.357580 * G + 0.180423 * B) / 0.950456
  Y <- 0.212671 * R + 0.715160 * G + 0.072169 * B
  Z <- (0.019334 * R + 0.119193 * G + 0.950227 * B) / 1.088754
  f <- function(t) {
    o <- 7.787 * t + 16 / 116
    i <- t > 0.008856
    o[i] <- t[i]^(1 / 3)
    o
  }
  fX <- f(X)
  fY <- f(Y)
  fZ <- f(Z)
  L <- 903.3 * Y
  i <- Y > 0.008856
  L[i] <- 116 * fY[i] - 16
  list(L = L, a = 500 * (fX - fY), b = 200 * (fY - fZ))
}

.fr_neutral <- function(img) {
  mx <- pmax(img[, , 1], img[, , 2], img[, , 3]) / 255
  mn <- pmin(img[, , 1], img[, , 2], img[, , 3]) / 255
  s <- ifelse(mx > 0, (mx - mn) / mx, 0)
  s < 0.2 & mx > 0.1 & mx < 0.98
}

.fr_de00 <- function(lab1, lab2) {
  lab1 <- matrix(lab1, ncol = 3)
  lab2 <- matrix(lab2, ncol = 3)
  L1 <- lab1[, 1]; a1 <- lab1[, 2]; b1 <- lab1[, 3]
  L2 <- lab2[, 1]; a2 <- lab2[, 2]; b2 <- lab2[, 3]
  rad <- pi / 180
  Cb <- (sqrt(a1^2 + b1^2) + sqrt(a2^2 + b2^2)) / 2
  G <- 0.5 * (1 - sqrt(Cb^7 / (Cb^7 + 25^7)))
  a1p <- (1 + G) * a1
  a2p <- (1 + G) * a2
  C1p <- sqrt(a1p^2 + b1^2)
  C2p <- sqrt(a2p^2 + b2^2)
  h1p <- (atan2(b1, a1p) / rad) %% 360
  h2p <- (atan2(b2, a2p) / rad) %% 360
  zero <- (C1p * C2p) == 0
  dh <- h2p - h1p
  dh <- ifelse(dh > 180, dh - 360, ifelse(dh < -180, dh + 360, dh))
  dh[zero] <- 0
  dLp <- L2 - L1
  dCp <- C2p - C1p
  dHp <- 2 * sqrt(C1p * C2p) * sin(dh / 2 * rad)
  Lbp <- (L1 + L2) / 2
  Cbp <- (C1p + C2p) / 2
  hs <- h1p + h2p
  hbp <- ifelse(abs(h1p - h2p) > 180, ifelse(hs < 360, (hs + 360) / 2, (hs - 360) / 2), hs / 2)
  hbp[zero] <- hs[zero]
  Tt <- 1 - 0.17 * cos((hbp - 30) * rad) + 0.24 * cos(2 * hbp * rad) +
    0.32 * cos((3 * hbp + 6) * rad) - 0.20 * cos((4 * hbp - 63) * rad)
  dtheta <- 30 * exp(-((hbp - 275) / 25)^2)
  RC <- 2 * sqrt(Cbp^7 / (Cbp^7 + 25^7))
  SL <- 1 + 0.015 * (Lbp - 50)^2 / sqrt(20 + (Lbp - 50)^2)
  SC <- 1 + 0.045 * Cbp
  SH <- 1 + 0.015 * Cbp * Tt
  RT <- -sin(2 * dtheta * rad) * RC
  sqrt((dLp / SL)^2 + (dCp / SC)^2 + (dHp / SH)^2 + RT * (dCp / SC) * (dHp / SH))
}

.fr_lab_to_hex <- function(lab) {
  lab <- matrix(lab, ncol = 3)
  fy <- (lab[, 1] + 16) / 116
  fx <- fy + lab[, 2] / 500
  fz <- fy - lab[, 3] / 200
  finv <- function(t) ifelse(t > 6 / 29, t^3, 3 * (6 / 29)^2 * (t - 4 / 29))
  X <- 0.950456 * finv(fx)
  Y <- finv(fy)
  Z <- 1.088754 * finv(fz)
  M <- solve(matrix(c(0.412453, 0.357580, 0.180423, 0.212671, 0.715160, 0.072169,
                      0.019334, 0.119193, 0.950227), 3, byrow = TRUE))
  rgb <- t(M %*% rbind(X, Y, Z))
  srgb <- .fr_to_srgb8(rgb) / 255
  ok <- stats::complete.cases(srgb)
  out <- rep(NA_character_, nrow(srgb))
  out[ok] <- grDevices::rgb(srgb[ok, 1], srgb[ok, 2], srgb[ok, 3])
  out
}
