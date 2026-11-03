test_that("masks and label maps are written and read", {
  m <- matrix(FALSE, 8, 6)
  m[2:4, 2:5] <- TRUE
  f <- tempfile(fileext = ".png")
  fr_write_mask(m, f)
  expect_identical(fr_read_mask(f), m)
  lab <- matrix(0L, 8, 6)
  lab[2:4, 2:3] <- 1L
  lab[6:7, 4:6] <- 2L
  g <- tempfile(fileext = ".png")
  png::writePNG(lab / 255, g)
  expect_identical(fr_read_labels(g), lab)
  expect_error(fr_read_image(tempfile()), "File not found")
})

test_that("example photos, crops and label maps are readable", {
  ex <- fr_example_photos()
  expect_true(all(file.exists(c(ex$photo, ex$crop, ex$labels))))
  img <- fr_read_image(ex$crop[1])
  lab <- fr_read_labels(ex$labels[1])
  expect_equal(dim(img)[1:2], dim(lab))
  expect_true(max(lab) >= 1)
  expect_equal(max(dim(fr_read_image(ex$photo[1], max_side = 100))[1:2]), 100)
})
