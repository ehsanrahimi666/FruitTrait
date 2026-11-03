#' Intraclass correlation ICC(1)
#'
#' One-way random-effects intraclass correlation for unbalanced groups
#' (Shrout and Fleiss ICC(1), with the average group size `k0` of the
#' unbalanced one-way ANOVA). Missing and infinite values and groups with fewer
#' than two values are dropped.
#'
#' @param values Numeric vector.
#' @param groups Grouping vector of the same length (for example the occurrence
#'   key, so that photos of the same plant on the same occasion form a group).
#' @return The ICC, or `NA` with fewer than three groups or no variance.
#' @examples
#' set.seed(1)
#' plant <- rep(1:30, each = 2)
#' true <- rnorm(30)[plant]
#' fr_icc(true + rnorm(60, sd = 0.3), plant)  # repeatable trait
#' fr_icc(rnorm(60), plant)                   # pure noise, near 0
#' @export
fr_icc <- function(values, groups) {
  if (length(values) != length(groups)) stop("`values` and `groups` differ in length.", call. = FALSE)
  ok <- is.finite(values) & !is.na(groups)
  v <- values[ok]
  g <- as.character(groups[ok])
  keep <- g %in% names(which(table(g) >= 2L))
  v <- v[keep]
  g <- g[keep]
  if (length(unique(g)) < 3L || stats::var(v) == 0) return(NA_real_)
  k <- tapply(v, g, length)
  m <- tapply(v, g, mean)
  N <- length(v)
  a <- length(k)
  msb <- sum(k * (m - mean(v))^2) / (a - 1)
  msw <- sum((v - m[g])^2) / (N - a)
  k0 <- (N - sum(k^2) / N) / (a - 1)
  den <- msb + (k0 - 1) * msw
  if (den > 0) (msb - msw) / den else NA_real_
}

#' Photo-to-photo repeatability of traits
#'
#' Estimates, for each trait, the ICC(1) between photos of the same observation
#' (rows sharing `group`), with a bootstrap confidence interval obtained by
#' resampling observations. Circular traits (hue angles) are rotated so that
#' their circular mean lies at 180 degrees, and each value is then expressed as
#' the circular mean of its group plus its circular deviation from that mean, so
#' that differences across 0/360 degrees are not inflated. Hue angles of fruits
#' that span the colour circle remain cut at one point of the circle, so their
#' ICC is best read together with that of a* and b*. This is the repeatability
#' reported in [fr_catalogue].
#'
#' @param x Data frame of traits with a grouping column, for example the
#'   measurable photos of [fr_validation] or of [fr_read_results()].
#' @param traits Trait columns. By default, all numeric photo traits of
#'   [fr_catalogue] found in `x`.
#' @param group Column that identifies the observation (several photos per group).
#' @param boot Number of bootstrap resamples (0 for none).
#' @param conf Confidence level of the interval.
#' @param seed Random seed for the bootstrap.
#' @return A data frame with `trait`, `icc`, `lower`, `upper`, `n_groups` and
#'   `n_photos`.
#' @examples
#' v <- fr_validation[fr_validation$both_measurable, ]
#' fr_repeatability(v, traits = c("m_L", "m_C", "m_hue"), boot = 50)
#' @export
fr_repeatability <- function(x, traits = NULL, group = "occurrenceKey", boot = 200,
                             conf = 0.95, seed = 1) {
  if (!is.data.frame(x)) stop("`x` must be a data frame.", call. = FALSE)
  if (!group %in% names(x)) stop("Column `", group, "` not found.", call. = FALSE)
  if (is.null(traits)) {
    ct <- .fr_data("fr_catalogue")
    traits <- intersect(ct$trait[ct$level == "photo" & ct$type != "categorical"], names(x))
  }
  miss <- setdiff(traits, names(x))
  if (length(miss)) stop("Traits not found: ", paste(miss, collapse = ", "), call. = FALSE)
  alpha <- (1 - conf) / 2
  rows <- lapply(traits, function(t) {
    v <- suppressWarnings(as.numeric(x[[t]]))
    g <- as.character(x[[group]])
    ok <- is.finite(v) & !is.na(g)
    v <- v[ok]
    g <- g[ok]
    multi <- g %in% names(which(table(g) >= 2L))
    v <- v[multi]
    g <- g[multi]
    if (t %in% .fr_circular) v <- .fr_circ_groups(v, g)
    est <- fr_icc(v, g)
    lo <- hi <- NA_real_
    if (boot > 0 && !is.na(est)) {
      sp <- split(v, g)
      bs <- .fr_with_seed(seed, vapply(seq_len(boot), function(b) {
        s <- sample(length(sp), replace = TRUE)
        vals <- unlist(sp[s], use.names = FALSE)
        fr_icc(vals, rep(seq_along(s), lengths(sp[s])))
      }, numeric(1)))
      q <- stats::quantile(bs, c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE, type = 7)
      lo <- q[1]
      hi <- q[2]
    }
    data.frame(trait = t, icc = est, lower = lo, upper = hi,
               n_groups = length(unique(g)), n_photos = length(v), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Augment a photo: camera changes and training-style transformations
#'
#' Changes a crop (and, for geometric changes, its fruit label map) in the ways
#' used to augment training images in computer vision and to test trait
#' invariance. Camera changes used for [fr_catalogue]: white balance (red and
#' blue gains in linear RGB, 0.75-1.25), exposure (linear gain 0.6-1.5), JPEG
#' re-compression (quality 30-89), Gaussian blur (sigma 0.5-1.5 px), rotation
#' (-30 to 30 degrees) and perspective (corners moved by up to 12% of the
#' shorter side). Training-style changes: rotation by 90, 180 or 270 degrees
#' (`"rotate90"`), mirroring (`"flip"`), and colour jitter of brightness,
#' contrast and saturation (factor 0.75-1.25) and hue (shift of up to 0.05 of the
#' hue circle), defined as in common augmentation libraries. Random values are
#' drawn unless `value` is given.
#'
#' @param img RGB array (values 0-255) or file path.
#' @param labels Fruit label map (integer matrix, 0 = background) or PNG path.
#' @param type Type of change; see Description.
#' @param value Optional fixed value: two gains (red, blue) for white balance;
#'   a factor for exposure, brightness, contrast or saturation; a quality
#'   (1-100) for JPEG; a sigma for blur; an angle in degrees for rotation; 1, 2
#'   or 3 quarter turns for `"rotate90"`; `"horizontal"` or `"vertical"` for
#'   `"flip"`; a shift (-0.5 to 0.5) for hue; or a 4 x 2 matrix of corner shifts
#'   (pixels) for perspective.
#' @return A list with `image` and `labels`.
#' @seealso [fr_augment_check()], [fr_invariance()]
#' @examples
#' ex <- fr_example_photos()
#' img <- fr_read_image(ex$crop[1]); lab <- fr_read_labels(ex$labels[1])
#' warm <- fr_augment(img, lab, "white_balance", value = c(1.2, 0.8))
#' turned <- fr_augment(img, lab, "rotate90", value = 1)
#' rbind(original = colMeans(fr_fruit_colour(img, lab)[, c("L", "a", "b")]),
#'       warmer = colMeans(fr_fruit_colour(warm$image, warm$labels)[, c("L", "a", "b")]),
#'       rotated = colMeans(fr_fruit_colour(turned$image, turned$labels)[, c("L", "a", "b")]))
#' @export
fr_augment <- function(img, labels, type = c("white_balance", "exposure", "jpeg", "blur", "rotate",
                                             "perspective", "rotate90", "flip", "brightness",
                                             "contrast", "saturation", "hue"), value = NULL) {
  type <- match.arg(type)
  img <- .fr_as_image(img)
  labels <- .fr_as_labels(labels)
  if (!identical(dim(labels), dim(img)[1:2])) stop("`labels` and `img` differ in size.", call. = FALSE)
  h <- nrow(labels)
  w <- ncol(labels)
  if (type %in% c("white_balance", "exposure")) {
    f <- if (type == "white_balance") {
      v <- value %||% stats::runif(2, 0.75, 1.25)
      c(v[1], 1, v[2])
    } else {
      rep(value %||% stats::runif(1, 0.6, 1.5), 3)
    }
    out <- img
    for (ch in 1:3) out[, , ch] <- .fr_to_srgb8(.fr_lin(img[, , ch]) * f[ch])
    return(list(image = out, labels = labels))
  }
  if (type == "jpeg") {
    q <- value %||% sample(30:89, 1)
    raw <- jpeg::writeJPEG(img / 255, target = raw(), quality = q / 100)
    return(list(image = round(jpeg::readJPEG(raw) * 255), labels = labels))
  }
  if (type == "blur") {
    s <- value %||% stats::runif(1, 0.5, 1.5)
    out <- img
    for (ch in 1:3) out[, , ch] <- round(.fr_gauss(img[, , ch], s))
    return(list(image = out, labels = labels))
  }
  if (type == "rotate90") {
    k <- as.integer(value %||% sample(1:3, 1)) %% 4L
    for (i in seq_len(k)) {
      img <- .fr_rot90(img)
      labels <- .fr_rot90(labels)
    }
    return(list(image = img, labels = labels))
  }
  if (type == "flip") {
    dir <- value %||% sample(c("horizontal", "vertical"), 1)
    if (!dir %in% c("horizontal", "vertical")) stop("`value` must be \"horizontal\" or \"vertical\".", call. = FALSE)
    if (dir == "horizontal") return(list(image = img[, w:1, , drop = FALSE], labels = labels[, w:1, drop = FALSE]))
    return(list(image = img[h:1, , , drop = FALSE], labels = labels[h:1, , drop = FALSE]))
  }
  if (type %in% c("brightness", "contrast", "saturation")) {
    f <- value %||% stats::runif(1, 0.75, 1.25)
    gray <- 0.2989 * img[, , 1] + 0.587 * img[, , 2] + 0.114 * img[, , 3]
    base <- switch(type, brightness = 0 * gray, contrast = 0 * gray + mean(gray), saturation = gray)
    out <- img
    for (ch in 1:3) out[, , ch] <- round(pmin(pmax(base + f * (img[, , ch] - base), 0), 255))
    return(list(image = out, labels = labels))
  }
  if (type == "hue") {
    shift <- value %||% stats::runif(1, -0.05, 0.05)
    return(list(image = .fr_hue_shift(img, shift), labels = labels))
  }
  if (type == "rotate") {
    ang <- (value %||% stats::runif(1, -30, 30)) * pi / 180
    al <- cos(ang)
    be <- sin(ang)
    cx <- w / 2
    cy <- h / 2
    H <- rbind(c(al, be, (1 - al) * cx - be * cy), c(-be, al, be * cx + (1 - al) * cy), c(0, 0, 1))
  } else {
    src <- rbind(c(0, 0), c(w, 0), c(w, h), c(0, h))
    j <- 0.12 * min(w, h)
    sh <- value %||% matrix(stats::runif(8, -j, j), 4, 2)
    H <- .fr_homography(src, src + sh)
  }
  .fr_warp(img, labels, H)
}

#' Trait invariance to camera changes and image resolution
#'
#' For each crop, measures the colour of every outlined fruit
#' ([fr_fruit_colour()]) on the original, on each simulated change
#' ([fr_augment()]) and on copies reduced so that the median fruit of the crop is
#' `rungs` pixels across, with the outlines held fixed. The ICC(1) between the
#' original and each changed version, across fruits, measures how invariant
#' each trait is. In the validation, traits were rated reliable when their
#' smallest photometric invariance was at least 0.8 (see [fr_catalogue]).
#'
#' @param images,labels Character vectors of file paths (or lists of arrays
#'   and label matrices) of crops and fruit label maps.
#' @param types Changes to apply; see [fr_augment()].
#' @param rungs Diameters (pixels) of the median fruit for the resolution ladder.
#' @param seed Random seed.
#' @param min_n Minimum number of fruits for an ICC.
#' @param min_px Fruits with fewer pixels are skipped.
#' @return A data frame of ICCs, one row per trait and one column per change;
#'   the per-fruit traits are in `attr(result, "data")`.
#' @examples
#' ex <- fr_example_photos()
#' inv <- fr_invariance(ex$crop[1:4], ex$labels[1:4], types = c("exposure", "blur"),
#'                      rungs = 16, min_n = 5)
#' round(inv, 2)
#' @export
fr_invariance <- function(images, labels, types = c("white_balance", "exposure", "jpeg", "blur",
                                                    "rotate", "perspective"),
                          rungs = c(8, 16, 32, 64), seed = 1, min_n = 10, min_px = 10) {
  if (length(images) != length(labels)) stop("Give one label map per image.", call. = FALSE)
  types <- vapply(types, function(t) match.arg(t, eval(formals(fr_augment)$type)), character(1))
  rows <- .fr_with_seed(seed, lapply(seq_along(images), function(i) {
    img <- .fr_as_image(if (is.list(images)) images[[i]] else images[i])
    lab <- .fr_as_labels(if (is.list(labels)) labels[[i]] else labels[i])
    one <- function(cond, im, lb) {
      f <- fr_fruit_colour(im, lb, min_px = min_px)
      if (!nrow(f)) return(NULL)
      cbind(id = paste(i, f$label, sep = ":"), condition = cond, f[, .fr_core_traits], stringsAsFactors = FALSE)
    }
    out <- list(one("original", img, lab))
    for (tp in types) {
      p <- fr_augment(img, lab, tp)
      out[[length(out) + 1L]] <- one(tp, p$image, p$labels)
    }
    px <- tabulate(lab[lab > 0])
    px <- px[px > 0]
    if (length(px)) {
      D <- 2 * sqrt(stats::median(px) / pi)
      for (r in rungs[rungs < D]) {
        s <- r / D
        nh <- max(8L, round(nrow(lab) * s))
        nw <- max(8L, round(ncol(lab) * s))
        out[[length(out) + 1L]] <- one(paste0("D", r), .fr_resize_image(img, nh, nw),
                                       .fr_resize_labels(lab, nh, nw))
      }
    }
    do.call(rbind, out)
  }))
  long <- do.call(rbind, rows)
  tr <- .fr_core_traits
  conds <- setdiff(unique(long$condition), "original")
  conds <- c(intersect(types, conds), setdiff(conds, types))
  icc <- vapply(conds, function(cd) {
    sub <- long[long$condition %in% c("original", cd), ]
    both <- names(which(tapply(sub$condition, sub$id, function(z) length(unique(z))) == 2))
    sub <- sub[sub$id %in% both, ]
    if (length(both) < min_n) return(stats::setNames(rep(NA_real_, length(tr)), tr))
    vapply(tr, function(t) {
      v <- sub[[t]]
      if (t %in% .fr_circular) v <- .fr_circ_groups(v, sub$id)
      fr_icc(v, sub$id)
    }, numeric(1))
  }, numeric(length(tr)))
  out <- as.data.frame(matrix(icc, nrow = length(tr), dimnames = list(tr, conds)))
  attr(out, "data") <- long
  out
}

#' Check trait stability under augmentation
#'
#' Applies augmentations of the kind used to train image models (rotations,
#' mirroring, colour jitter) and camera changes ([fr_augment()]) to a crop,
#' measures the colour of its fruits again and reports how much each trait
#' moves. Changes are expressed relative to the between-photo standard
#' deviation of the trait among the measurable photos of [fr_validation], so
#' that a value of 0.25 means a quarter of the usual difference between photos.
#' Rotations and mirroring do not change colour apart from resampling; colour
#' jitter shows how sensitive a trait is to camera processing.
#'
#' @param img RGB array or file path; several paths process several crops.
#' @param labels Label map or file path (one per image).
#' @param types Augmentations to apply; see [fr_augment()].
#' @param n Random draws per augmentation.
#' @param tol Relative change above which a trait is flagged (a screening
#'   default, not a validated limit).
#' @param seed Random seed.
#' @return A data frame with `type`, `trait`, `original` (mean over the fruits
#'   of the crop), `mean_change` (mean absolute change), `rel_change` (largest
#'   absolute change divided by the between-photo standard deviation) and
#'   `flag`, with `photo_id` for file input.
#' @examples
#' ex <- fr_example_photos()
#' chk <- fr_augment_check(ex$crop[1], ex$labels[1], types = c("rotate90", "hue", "saturation"))
#' chk[chk$trait %in% c("L", "C", "hue_deg"), ]
#' @export
fr_augment_check <- function(img, labels, types = c("rotate90", "flip", "rotate", "brightness",
                                                    "contrast", "saturation", "hue", "white_balance",
                                                    "exposure", "jpeg", "blur"),
                             n = 1, tol = 0.25, seed = 1) {
  if (is.character(img) && length(img) > 1L) {
    if (length(labels) != length(img)) stop("Give one label map per image.", call. = FALSE)
    rows <- lapply(seq_along(img), function(i) fr_augment_check(img[i], labels[i], types, n, tol, seed + i))
    return(do.call(rbind, rows))
  }
  id <- if (is.character(img)) tools::file_path_sans_ext(basename(img)) else NULL
  img <- .fr_as_image(img)
  labels <- .fr_as_labels(labels)
  tr <- .fr_core_traits
  photo_traits <- function(im, lb) {
    f <- fr_fruit_colour(im, lb)
    if (!nrow(f)) return(stats::setNames(rep(NA_real_, length(tr)), tr))
    L <- mean(f$L)
    a <- mean(f$a)
    b <- mean(f$b)
    c(L = L, a = a, b = b, C = sqrt(a^2 + b^2), hue_deg = (atan2(b, a) * 180 / pi) %% 360)
  }
  base <- photo_traits(img, labels)
  if (all(is.na(base))) stop("No fruit is large enough for colour.", call. = FALSE)
  ref <- .fr_ref_sd()
  rows <- .fr_with_seed(seed, lapply(types, function(tp) {
    d <- t(vapply(seq_len(n), function(k) {
      a <- fr_augment(img, labels, tp)
      v <- photo_traits(a$image, a$labels)
      dv <- abs(v - base)
      dv["hue_deg"] <- pmin(dv["hue_deg"], 360 - dv["hue_deg"])
      dv
    }, numeric(length(tr))))
    data.frame(type = tp, trait = tr, original = unname(base), mean_change = colMeans(d),
               rel_change = apply(d, 2, max) / ref[tr], stringsAsFactors = FALSE)
  }))
  out <- do.call(rbind, rows)
  out$flag <- out$rel_change > tol
  rownames(out) <- NULL
  if (!is.null(id)) out <- cbind(photo_id = id, out, stringsAsFactors = FALSE)
  out
}

.fr_core_traits <- c("L", "a", "b", "C", "hue_deg")
.fr_circular <- c("hue_deg", "m_hue", "v_hue", "hue")

# Between-photo SD of the photo colour among measurable validation photos (first photo of each observation).
.fr_ref_sd <- function() {
  v <- .fr_data("fr_validation")
  v <- v[v$measurable & v$position == "first", ]
  hue <- .fr_rotate(v$m_hue)
  c(L = stats::sd(v$m_L, na.rm = TRUE), a = stats::sd(v$m_a, na.rm = TRUE), b = stats::sd(v$m_b, na.rm = TRUE),
    C = stats::sd(v$m_C, na.rm = TRUE), hue_deg = stats::sd(hue, na.rm = TRUE))
}

# Hue angles for an ICC: rotated so that their circular mean lies at 180 degrees, then the circular mean of
# each group plus the circular deviation of each value from it.
.fr_circ_groups <- function(v, g) {
  v <- .fr_rotate(v)
  ok <- is.finite(v) & !is.na(g)
  if (!any(ok)) return(v)
  gg <- as.character(g[ok])
  r <- v[ok] * pi / 180
  cm <- (atan2(tapply(sin(r), gg, mean), tapply(cos(r), gg, mean)) * 180 / pi) %% 360
  m <- unname(cm[gg])
  v[ok] <- m + ((v[ok] - m + 180) %% 360) - 180
  v
}

.fr_rotate <- function(v) {
  r <- v[is.finite(v)] * pi / 180
  if (!length(r)) return(v)
  cm <- atan2(mean(sin(r)), mean(cos(r))) * 180 / pi
  (v - cm + 180) %% 360
}

.fr_resize_image <- function(img, h, w) {
  out <- array(0, c(h, w, 3L))
  for (ch in 1:3) out[, , ch] <- .fr_area_resize(img[, , ch], h, w)
  round(out)
}

# Label map resized by area: a pixel takes the label covering most of it, if more than half.
.fr_resize_labels <- function(lab, h, w) {
  ids <- sort(setdiff(unique(as.vector(lab)), 0L))
  out <- matrix(0L, h, w)
  best <- matrix(0.5, h, w)
  for (k in ids) {
    f <- .fr_area_resize((lab == k) * 1, h, w)
    sel <- f > best
    out[sel] <- k
    best[sel] <- f[sel]
  }
  out
}

# Quarter turn (clockwise) of a matrix or an h x w x 3 array.
.fr_rot90 <- function(x) {
  if (length(dim(x)) == 2L) return(t(x[nrow(x):1, , drop = FALSE]))
  out <- array(0, c(dim(x)[2], dim(x)[1], 3L))
  for (ch in 1:3) out[, , ch] <- t(x[dim(x)[1]:1, , ch, drop = TRUE])
  out
}

# Hue shift through HSV, as in common colour-jitter augmentation.
.fr_hue_shift <- function(img, shift) {
  r <- img[, , 1] / 255
  g <- img[, , 2] / 255
  b <- img[, , 3] / 255
  mx <- pmax(r, g, b)
  mn <- pmin(r, g, b)
  d <- mx - mn
  hh <- numeric(length(r))
  i <- d > 0 & mx == r
  hh[i] <- ((g[i] - b[i]) / d[i]) %% 6
  i2 <- d > 0 & mx == g & !i
  hh[i2] <- (b[i2] - r[i2]) / d[i2] + 2
  i3 <- d > 0 & mx == b & !i & !i2
  hh[i3] <- (r[i3] - g[i3]) / d[i3] + 4
  hh <- (hh / 6 + shift) %% 1
  s <- ifelse(mx > 0, d / mx, 0)
  v <- mx
  hp <- hh * 6
  cc <- v * s
  x <- cc * (1 - abs(hp %% 2 - 1))
  m <- v - cc
  seg <- floor(hp) %% 6
  rr <- ifelse(seg %in% c(0, 5), cc, ifelse(seg %in% c(1, 4), x, 0))
  gg <- ifelse(seg %in% c(1, 2), cc, ifelse(seg %in% c(0, 3), x, 0))
  bb <- ifelse(seg %in% c(3, 4), cc, ifelse(seg %in% c(2, 5), x, 0))
  out <- img
  out[, , 1] <- round(255 * (rr + m))
  out[, , 2] <- round(255 * (gg + m))
  out[, , 3] <- round(255 * (bb + m))
  out
}

.fr_gauss <- function(M, sigma) {
  k <- bitwOr(as.integer(round(sigma * 6 + 1)), 1L)
  r <- (k - 1) %/% 2
  wts <- exp(-((-r):r)^2 / (2 * sigma^2))
  wts <- wts / sum(wts)
  refl <- function(n) {
    i <- (-r):(n - 1 + r)
    if (n == 1L) return(rep(1L, length(i)))
    while (any(i < 0 | i > n - 1)) {
      i[i < 0] <- -i[i < 0]
      i[i > n - 1] <- 2 * (n - 1) - i[i > n - 1]
    }
    i + 1L
  }
  h <- nrow(M)
  w <- ncol(M)
  P <- M[, refl(w), drop = FALSE]
  o <- matrix(0, h, w)
  for (d in seq_along(wts)) o <- o + wts[d] * P[, d:(d + w - 1), drop = FALSE]
  P <- o[refl(h), , drop = FALSE]
  o <- matrix(0, h, w)
  for (d in seq_along(wts)) o <- o + wts[d] * P[d:(d + h - 1), , drop = FALSE]
  o
}

.fr_homography <- function(src, dst) {
  A <- matrix(0, 8, 8)
  b <- numeric(8)
  for (i in 1:4) {
    x <- src[i, 1]
    y <- src[i, 2]
    u <- dst[i, 1]
    v <- dst[i, 2]
    A[i, ] <- c(x, y, 1, 0, 0, 0, -x * u, -y * u)
    A[i + 4, ] <- c(0, 0, 0, x, y, 1, -x * v, -y * v)
    b[i] <- u
    b[i + 4] <- v
  }
  matrix(c(solve(A, b), 1), 3, 3, byrow = TRUE)
}

# Inverse-mapping warp: bilinear interpolation with reflected borders for the
# image, nearest neighbour with an empty border for the label map.
.fr_warp <- function(img, labels, H) {
  h <- nrow(labels)
  w <- ncol(labels)
  Hi <- solve(H)
  gx <- rep(0:(w - 1), each = h)
  gy <- rep(0:(h - 1), times = w)
  den <- Hi[3, 1] * gx + Hi[3, 2] * gy + Hi[3, 3]
  sx <- (Hi[1, 1] * gx + Hi[1, 2] * gy + Hi[1, 3]) / den
  sy <- (Hi[2, 1] * gx + Hi[2, 2] * gy + Hi[2, 3]) / den
  refl <- function(i, n) {
    for (it in 1:3) {
      i[i < 0] <- -i[i < 0] - 1
      i[i > n - 1] <- 2 * n - 1 - i[i > n - 1]
    }
    pmin(pmax(i, 0), n - 1)
  }
  x0 <- floor(sx)
  y0 <- floor(sy)
  fx <- sx - x0
  fy <- sy - y0
  X0 <- refl(x0, w)
  X1 <- refl(x0 + 1, w)
  Y0 <- refl(y0, h)
  Y1 <- refl(y0 + 1, h)
  out <- img
  for (ch in 1:3) {
    M <- img[, , ch]
    v <- (1 - fx) * (1 - fy) * M[cbind(Y0 + 1, X0 + 1)] + fx * (1 - fy) * M[cbind(Y0 + 1, X1 + 1)] +
      (1 - fx) * fy * M[cbind(Y1 + 1, X0 + 1)] + fx * fy * M[cbind(Y1 + 1, X1 + 1)]
    out[, , ch] <- matrix(round(v), h, w)
  }
  nx <- round(sx)
  ny <- round(sy)
  inside <- nx >= 0 & nx <= w - 1 & ny >= 0 & ny <= h - 1
  lv <- integer(length(nx))
  lv[inside] <- labels[cbind(ny[inside] + 1, nx[inside] + 1)]
  list(image = out, labels = matrix(lv, h, w))
}
