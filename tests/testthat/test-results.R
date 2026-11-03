test_that("example results are read with their fruit table", {
  res <- fr_example_results()
  expect_s3_class(res, "fr_results")
  f <- fr_fruits(res)
  expect_true(all(c("photo_id", "fruit", "label", "ok", "L", "a", "b", "name", "outline_xy") %in% names(f)))
  expect_true(all(f$photo_id %in% res$photo_id))
  expect_true(all(fr_fruits(res, "outlined")$ok))
  expect_equal(sum(res$keep), sum(res$mask_ok %in% TRUE))
  sub <- res[1:2, ]
  expect_true(all(fr_fruits(sub)$photo_id %in% sub$photo_id))
  expect_output(print(res), "fr_results")
  expect_output(print(summary(res)), "Fruits")
})

test_that("the R colour of outlined fruits agrees with the back end", {
  res <- fr_example_results()
  ex <- fr_example_photos()
  f <- fr_fruits(res, "outlined")
  for (i in seq_len(nrow(ex))) {
    fc <- fr_fruit_colour(ex$crop[i], ex$labels[i])
    fb <- f[f$photo_id == ex$photo_id[i], ]
    fb <- fb[match(fc$label, fb$label), ]
    # the crops are stored as JPEG, so a small difference to the colours of the back end is expected
    expect_lt(max(abs(fc$L - fb$L)), 1.5)
    expect_lt(max(fr_delta_e(cbind(fc$L, fc$a, fc$b), cbind(fb$L, fb$a, fb$b))), 2)
  }
})

test_that("decision rules and summaries", {
  res <- fr_example_results()
  r2 <- fr_filter(res, "none")
  expect_true(all(r2$keep))
  r3 <- fr_filter(res, "outlined")
  expect_equal(r3$keep, res$mask_ok %in% TRUE)
  s <- fr_summarise(res, which = "outlined")
  expect_true(all(c("species", "n_photos", "n_fruits", "L", "a", "b", "C", "hue_deg", "name") %in% names(s)))
  expect_equal(sum(s$n_fruits), nrow(fr_fruits(res, "outlined")[fr_fruits(res, "outlined")$photo_id %in%
                                                                     res$photo_id[!is.na(res$species)], ]))
  expect_true(all(abs(rowSums(s[, grep("^share_", names(s))]) - 1) < 1e-9))
})

test_that("plots draw without error", {
  pdf(NULL)
  on.exit(dev.off())
  res <- fr_example_results()
  ex <- fr_example_photos()
  expect_invisible(fr_plot_fruits(ex$crop[1], ex$labels[1], main = "test"))
  expect_s3_class(fr_plot_colours(res, which = "outlined"), "data.frame")
  expect_s3_class(fr_contact_sheet(res, ncol = 3), "data.frame")
})
