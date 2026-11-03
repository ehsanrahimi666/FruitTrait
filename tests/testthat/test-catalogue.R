test_that("the catalogue covers the traits of the back end and of R", {
  cat <- fr_catalogue
  expect_equal(anyDuplicated(cat$trait), 0L)
  expect_true(all(cat$level %in% c("photo", "fruit")))
  expect_true(all(nzchar(cat$description)))
  expect_true(all(as.character(cat$verdict) %in% levels(cat$verdict)))
  res <- fr_example_results()
  expect_true(all(cat$trait[cat$level == "photo"] %in% names(res)))
  expect_true(all(cat$trait[cat$level == "photo"] %in% names(fr_validation)))
  expect_true(all(cat$trait[cat$level == "fruit"] %in% names(fr_fruits(res))))
  ex <- fr_example_photos()
  r_traits <- names(fr_fruit_colour(ex$crop[1], ex$labels[1]))
  expect_true(all(cat$trait[cat$level == "fruit" & cat$computed_by == "R and back end"] %in% r_traits))
})

test_that("verdicts follow the stated rule", {
  x <- fr_catalogue[fr_catalogue$type %in% c("numeric", "circular"), ]
  inv <- x$inv_photometric_min
  rel <- x$icc_measurable >= 0.6 & !is.na(inv) & inv >= 0.8 & !is.na(x$icc_D32) & x$icc_D32 >= 0.8
  care <- !rel & x$icc_measurable >= 0.4 & (is.na(inv) | inv >= 0.7)
  expect_equal(as.character(x$verdict), ifelse(rel, "reliable", ifelse(care, "use with care", "not reliable")))
  nm <- fr_catalogue[fr_catalogue$type == "categorical", ]
  expect_equal(as.character(nm$verdict), ifelse(nm$agreement >= 0.8, "reliable",
                                                ifelse(nm$agreement >= 0.6, "use with care", "not reliable")))
  pr <- fr_catalogue[fr_catalogue$type == "probability", ]
  ok <- fr_benchmark$heads$usable[match(sub("p_", "", pr$trait), fr_benchmark$heads$head)]
  expect_equal(as.character(pr$verdict), ifelse(ok, "reliable", "not reliable"))
})

test_that("traits are looked up and selected", {
  i <- fr_trait_info(c("m_hue", "m_L"))
  expect_equal(i$trait, c("m_hue", "m_L"))
  expect_true(all(fr_trait_info(level = "fruit")$level == "fruit"))
  expect_true(all(fr_trait_info(family = "colour")$family == "colour"))
  expect_true(all(fr_trait_info(verdict = "reliable")$verdict == "reliable"))
  expect_named(fr_trait_info("m_L", columns = c("trait", "unit")), c("trait", "unit"))
  expect_equal(ncol(fr_trait_info("m_L", columns = NULL)), ncol(fr_catalogue))
  expect_error(fr_trait_info("nope"), "Unknown trait")
  expect_error(fr_trait_info(columns = "nope"), "Unknown column")
  rel <- fr_reliable_traits()
  expect_true(all(as.character(fr_catalogue$verdict[match(rel, fr_catalogue$trait)]) == "reliable"))
  expect_length(fr_reliable_traits(fruit_px = 1), 0)
  both <- fr_reliable_traits(c("reliable", "use with care"), level = c("photo", "fruit"))
  expect_true(all(rel %in% both))
  big <- fr_reliable_traits(c("reliable", "use with care"), level = c("photo", "fruit"), fruit_px = 64)
  expect_true(all(big %in% both))
})
