test_that("ICC(1) behaves as expected", {
  set.seed(2)
  g <- rep(1:40, each = 2)
  true <- rnorm(40)[g]
  expect_gt(fr_icc(true + rnorm(80, sd = 0.2), g), 0.9)
  expect_lt(abs(fr_icc(rnorm(80), g)), 0.35)
  expect_true(is.na(fr_icc(c(1, 2), c(1, 1))))
  expect_true(is.na(fr_icc(rep(1, 10), rep(1:5, 2))))
  v <- c(1, 1.1, 2, 2.2, 2.1, 3, 3.1, NA, 5)
  gg <- c(1, 1, 2, 2, 2, 3, 3, 4, 5)
  expect_gt(fr_icc(v, gg), 0.9)
  expect_error(fr_icc(1:3, 1:2), "differ in length")
})

test_that("ICC(1) equals the one-way ANOVA estimate for balanced groups", {
  set.seed(4)
  g <- rep(1:25, each = 3)
  v <- rnorm(25)[g] + rnorm(75, sd = 0.5)
  a <- stats::anova(stats::lm(v ~ factor(g)))
  msb <- a[1, "Mean Sq"]
  msw <- a[2, "Mean Sq"]
  expect_equal(fr_icc(v, g), (msb - msw) / (msb + 2 * msw))
})

test_that("hue repeatability is not inflated across 0/360 degrees", {
  set.seed(3)
  n <- 40
  base <- runif(n, 0, 360)
  d <- data.frame(occurrenceKey = rep(seq_len(n), each = 2),
                  m_hue = (rep(base, each = 2) + rnorm(2 * n, sd = 3)) %% 360,
                  m_L = rep(rnorm(n, 50, 10), each = 2) + rnorm(2 * n))
  r <- fr_repeatability(d, traits = c("m_hue", "m_L"), boot = 50)
  expect_equal(r$n_groups, c(n, n))
  expect_gt(min(r$icc), 0.9)
  expect_true(all(r$lower <= r$icc & r$icc <= r$upper))
  h <- FruitTrait:::.fr_circ_groups(c(10, 350, 170, 190, 100, 104), c(1, 1, 2, 2, 3, 3))
  expect_equal(abs(h[1] - h[2]), 20)
  expect_equal(abs(h[5] - h[6]), 4)
  expect_error(fr_repeatability(d, group = "nope"), "not found")
  expect_error(fr_repeatability(d, traits = "nope"), "not found")
  expect_error(fr_repeatability(as.list(d)), "data frame")
})

test_that("photometric augmentations keep the outlines and the image size", {
  ex <- fr_example_photos()
  img <- fr_read_image(ex$crop[1])
  lab <- fr_read_labels(ex$labels[1])
  set.seed(5)
  for (tp in c("white_balance", "exposure", "jpeg", "blur", "brightness", "contrast", "saturation", "hue")) {
    a <- fr_augment(img, lab, tp)
    expect_identical(a$labels, lab)
    expect_equal(dim(a$image), dim(img))
    expect_true(all(a$image >= 0 & a$image <= 255))
  }
  expect_equal(fr_augment(img, lab, "exposure", value = 1)$image, img)
  warm <- fr_augment(img, lab, "white_balance", value = c(1.2, 0.8))$image
  expect_gt(mean(warm[, , 1]), mean(img[, , 1]))
  expect_lt(mean(warm[, , 3]), mean(img[, , 3]))
  expect_error(fr_augment(img, lab[-1, ], "blur"), "differ in size")
})

test_that("geometric augmentations move the outlines with the image", {
  ex <- fr_example_photos()
  img <- fr_read_image(ex$crop[2])
  lab <- fr_read_labels(ex$labels[2])
  r <- fr_augment(img, lab, "rotate90", value = 1)
  expect_equal(dim(r$labels), rev(dim(lab)))
  im4 <- img
  lb4 <- lab
  for (i in 1:4) {
    z <- fr_augment(im4, lb4, "rotate90", value = 1)
    im4 <- z$image
    lb4 <- z$labels
  }
  expect_identical(lb4, lab)
  expect_equal(im4, img)
  f <- fr_augment(img, lab, "flip", value = "horizontal")
  expect_identical(f$labels, lab[, ncol(lab):1])
  expect_error(fr_augment(img, lab, "flip", value = "diagonal"), "horizontal")
  same <- fr_augment(img, lab, "rotate", value = 0)
  expect_equal(same$image, img)
  expect_identical(same$labels, lab)
  set.seed(6)
  for (tp in c("rotate", "perspective")) {
    a <- fr_augment(img, lab, tp)
    expect_equal(dim(a$image), dim(img))
    expect_true(all(unique(as.vector(a$labels)) %in% c(0, unique(as.vector(lab)))))
  }
})

test_that("invariance runs on the example crops", {
  ex <- fr_example_photos()
  inv <- fr_invariance(ex$crop, ex$labels, types = c("exposure", "jpeg", "rotate"), rungs = 32, min_n = 5)
  expect_true(all(c("exposure", "jpeg", "rotate") %in% names(inv)))
  expect_equal(rownames(inv), c("L", "a", "b", "C", "hue_deg"))
  expect_gt(inv["a", "jpeg"], 0.95)
  d <- attr(inv, "data")
  expect_true(all(c("id", "condition", "L", "a", "b") %in% names(d)))
  expect_true(all(d$condition %in% c("original", "exposure", "jpeg", "rotate", "D32")))
  expect_error(fr_invariance(ex$crop[1:2], ex$labels[1]), "one label map")
})

test_that("the augmentation check reports changes relative to the between-photo SD", {
  ex <- fr_example_photos()
  chk <- fr_augment_check(ex$crop[1], ex$labels[1], types = c("rotate90", "flip", "saturation"))
  expect_equal(nrow(chk), 15)
  expect_true(all(c("photo_id", "type", "trait", "original", "mean_change", "rel_change", "flag") %in% names(chk)))
  expect_true(all(chk$mean_change[chk$type %in% c("rotate90", "flip")] < 1e-6))
  expect_equal(chk$flag, chk$rel_change > 0.25)
  two <- fr_augment_check(ex$crop[1:2], ex$labels[1:2], types = "flip")
  expect_equal(unique(two$photo_id), ex$photo_id[1:2])
  empty <- matrix(0L, 20, 20)
  expect_error(fr_augment_check(array(100, c(20, 20, 3)), empty), "No fruit")
})

test_that("the repeatability plot draws", {
  pdf(NULL)
  on.exit(dev.off())
  r <- fr_repeatability(fr_validation[fr_validation$both_measurable, ], traits = c("m_L", "m_C"), boot = 20)
  expect_identical(fr_plot_repeatability(r)$trait, r$trait[order(r$icc)])
  expect_error(fr_plot_repeatability(data.frame(x = 1)), "fr_repeatability")
})
