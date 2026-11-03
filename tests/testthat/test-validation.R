test_that("pairs reproduce the repeatability of the published data set", {
  v <- fr_validation
  expect_equal(nrow(v), 3398)
  expect_true(all(table(v$occurrenceKey) == 2))
  expect_equal(length(unique(v$occurrenceKey)), 1699)
  first <- v[v$position == "first", ]
  second <- v[v$position == "second", ][match(first$occurrenceKey, v$occurrenceKey[v$position == "second"]), ]
  expect_equal(round(mean(first$measurable == second$measurable), 3), 0.704)
  expect_equal(sum(first$measurable & second$measurable), 633)
  expect_equal(sum(xor(first$measurable, second$measurable)), 503)
  expect_equal(first$measurable & second$measurable, first$both_measurable)
  m <- v[v$both_measurable, ]
  r <- fr_repeatability(m, traits = c("m_L", "m_a", "m_b", "m_C"), boot = 0)
  expect_equal(round(r$icc, 3), c(0.759, 0.902, 0.880, 0.883))
  expect_equal(r$icc, fr_catalogue$icc_measurable[match(r$trait, fr_catalogue$trait)], tolerance = 1e-12)
  expect_equal(round(fr_catalogue$agreement[fr_catalogue$trait == "m_name"], 3), 0.673)
})

test_that("photo colour follows the reflectance spectra of the reference species", {
  r <- fr_reference
  expect_equal(nrow(r), 104)
  sp <- function(x, y) round(stats::cor(x, y, method = "spearman"), 3)
  expect_equal(sp(r$C, r$spectral_saturation), 0.616)
  expect_equal(sp(r$L, r$spectral_lightness), 0.327)
  expect_equal(sp((r$hue_deg + 30) %% 360 - 30, r$spectral_hue_nm), -0.060)
  nm <- c(pink = "red", purple = "black")
  pn <- ifelse(r$photo_name %in% names(nm), nm[r$photo_name], r$photo_name)
  expect_equal(round(mean(pn == r$spectrum_colour), 3), 0.462)
})

test_that("benchmark tables are complete", {
  b <- fr_benchmark
  expect_equal(nrow(b$photos), 479)
  expect_equal(nrow(b$outlines), 1367)
  expect_equal(sum(b$photos$n_outlined), 2060)
  expect_equal(sum(b$photos$n_passed), nrow(b$outlines))
  expect_equal(sum(b$photos$n_passed > 0), 388)
  reasons <- rowSums(b$photos[, c("n_small", "n_fill", "n_shape", "n_border", "n_overlap", "n_size")])
  failed <- b$photos$n_outlined - b$photos$n_passed
  expect_equal(sum(failed), 693)
  expect_true(all(failed <= reasons & (failed > 0) == (reasons > 0)))
  expect_true(all(b$outlines$photo_id %in% b$photos$photo_id))
  expect_true(all(b$outlines$cls %in% c("F", "Y", "P", "C", "N")))
  expect_true(all(is.na(b$outlines$stage[!b$outlines$cls %in% c("F", "Y")])))
  expect_equal(b$heads$head, c("valid", "ripe", "dry"))
  expect_equal(b$heads$usable, c(TRUE, TRUE, FALSE))
  expect_equal(b$heads$fruits[1], 1367)
  expect_true(all(b$heads$usable == (b$heads$cv_auc >= 0.8 & b$heads$oof_balanced_accuracy >= 0.75)))
  k <- b$rules[b$rules$threshold == 0.5, ]
  o <- b$outlines
  kv <- o$p_valid >= 0.5
  expect_equal(k$valid_kept, sum(kv))
  expect_equal(k$valid_precision, mean(o$cls[kv] %in% c("F", "Y")))
})

test_that("R and the back end give the same colour on the benchmark crops", {
  a <- fr_benchmark$r_python
  skip_if(is.null(a), "agreement table not built")
  d <- fr_delta_e(cbind(a$L_r, a$a_r, a$b_r), cbind(a$L_py, a$a_py, a$b_py))
  expect_lt(stats::quantile(d, 0.99, na.rm = TRUE), 1)
  expect_gt(mean(a$name_r == a$name_py, na.rm = TRUE), 0.95)
})
