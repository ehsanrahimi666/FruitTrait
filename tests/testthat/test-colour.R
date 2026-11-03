test_that("sRGB to CIELAB agrees with the back end (OpenCV)", {
  rgb <- rbind(c(150, 20, 35), c(60, 70, 110), c(250, 210, 30), c(255, 255, 255), c(0, 0, 0), c(30, 120, 40))
  py <- rbind(c(32.051018, 51.37357, 27.799202), c(30.559818, 7.559821, -24.455654),
              c(85.258484, -1.178473, 82.1051), c(100, 0, 0), c(0, 0, 0), c(44.03355, -43.03156, 35.50166))
  lab <- fr_rgb_to_lab(rgb)
  expect_equal(unname(lab), py, tolerance = 1e-4)
  expect_equal(fr_colour_name(lab), c("red", "blue", "yellow", "white", "black", "green"))
  img <- array(rep(c(150, 20, 35), each = 4), c(2, 2, 3))
  expect_equal(fr_rgb_to_lab(img)[1, 1, ], py[1, ], tolerance = 1e-4)
})

test_that("colour names follow the rule", {
  expect_equal(fr_colour_name(c(70, 50, 20), c(2, 0, 1), c(1, 3, -2)), c("white", "grey", "black"))
  expect_equal(fr_colour_name(40, 5, -30), "blue")
  expect_equal(fr_colour_name(35, 30, -15), "purple")
  expect_error(fr_colour_name(1:2, 1, 1))
})

test_that("CIEDE2000 reproduces a published test pair", {
  # Sharma, Wu and Dalal (2005), pair 1
  expect_equal(fr_delta_e(c(50, 2.6772, -79.7751), c(50, 0, -82.7485)), 2.0425, tolerance = 1e-4)
  expect_equal(fr_delta_e(c(40, 10, 10), c(40, 10, 10)), 0)
})

test_that("spectra are converted to CIELAB", {
  wl <- 360:830
  lab <- fr_spectra_to_lab(wl, cbind(white = rep(1, length(wl)), grey = rep(0.18, length(wl))))
  expect_equal(unname(lab["white", c("L", "a", "b")]), c(100, 0, 0), tolerance = 1e-6)
  expect_equal(unname(lab["grey", "L"]), 49.496, tolerance = 1e-3)
  expect_lt(abs(lab["grey", "a"]) + abs(lab["grey", "b"]), 1e-6)
  pct <- fr_spectra_to_lab(wl, rep(18, length(wl)))
  expect_equal(unname(pct[1, "L"]), unname(lab["grey", "L"]))
  red <- 4 + 60 / (1 + exp(-(300:700 - 600) / 10))
  expect_equal(fr_colour_name(fr_spectra_to_lab(300:700, red)[, 1:3, drop = FALSE]), "red")
})

test_that("fruit colour is the mean without the brightest 2% of pixels", {
  img <- array(0, c(20, 20, 3))
  img[, , 1] <- 150; img[, , 2] <- 20; img[, , 3] <- 35
  img[1, 1, ] <- 255
  lab <- matrix(0L, 20, 20)
  lab[1:10, ] <- 1L
  lab[11:20, 1:10] <- 2L
  fc <- fr_fruit_colour(img, lab)
  expect_equal(fc$label, 1:2)
  expect_equal(fc$px, c(200L, 100L))
  ref <- fr_rgb_to_lab(rbind(c(150, 20, 35)))
  expect_equal(fc$L, rep(unname(ref[1, "L"]), 2), tolerance = 1e-6)
  expect_equal(fc$name, c("red", "red"))
  expect_equal(nrow(fr_fruit_colour(img, matrix(0L, 20, 20))), 0L)
  expect_error(fr_fruit_colour(img, matrix(0L, 5, 5)), "differ in size")
})

test_that("white balance keeps neutral photos and corrects a cast", {
  grey <- array(128, c(10, 10, 3))
  expect_equal(fr_white_balance(grey, "gray_world"), grey)
  cast <- grey
  cast[, , 3] <- 150  # saturation 0.15: still near-neutral, so the cast can be estimated
  wb <- fr_white_balance(cast, "gray_world")
  expect_lt(abs(wb[1, 1, 1] - wb[1, 1, 3]), 2)
  chk <- fr_colour_check(cast)
  expect_true(chk$cast > 0)
  strong <- grey
  strong[, , 3] <- 200  # no near-neutral pixel left: the cast cannot be estimated
  expect_true(is.na(fr_colour_check(strong)$cast))
})
