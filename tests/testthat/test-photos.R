test_that("iNaturalist observations are parsed", {
  js <- jsonlite::fromJSON(test_path("fixtures", "inat_observations.json"), simplifyVector = FALSE)
  d <- FruitTrait:::.fr_parse_inat(js$results, c("cc0", "cc-by", "cc-by-nc"), 1)
  expect_equal(nrow(d), 2L)
  expect_true(all(grepl("/original\\.", d$url)))
  expect_equal(d$license, c("cc-by", "cc0"))
  expect_equal(d$latitude, c(52.1, -33.9))
  d2 <- FruitTrait:::.fr_parse_inat(js$results, "cc0", 1)
  expect_equal(nrow(d2), 1L)
})

test_that("GBIF occurrences are parsed", {
  js <- jsonlite::fromJSON(test_path("fixtures", "gbif_occurrences.json"), simplifyVector = FALSE)
  d <- FruitTrait:::.fr_parse_occurrences(js$results, c("CC0_1_0", "CC_BY_4_0"), 1)
  expect_equal(nrow(d), 1L)
  expect_equal(d$license, "CC_BY_4_0")
  expect_match(d$reproductiveCondition, "fruits")
})

test_that("scope look-up and attribution", {
  sc <- fr_in_scope(c("Sorbus aucuparia", "Fragaria vesca", "Not a species"))
  expect_equal(sc$in_scope, c(TRUE, FALSE, FALSE))
  expect_equal(sc$family[1], "Rosaceae")
  expect_true(all(fr_scope$woody_source %in% c("woodiness database", "WCVP life form")))
  a <- fr_attribution(fr_example_photos())
  expect_true(all(grepl("Photo: ", a)))
})

test_that("photo folders are listed", {
  d <- file.path(tempdir(), "fr_folder_test")
  ex <- fr_example_photos()
  dir.create(file.path(d, "Sorbus_aucuparia"), recursive = TRUE, showWarnings = FALSE)
  file.copy(ex$photo[1], file.path(d, "Sorbus_aucuparia", "a.jpg"), overwrite = TRUE)
  x <- fr_read_folder(d)
  expect_equal(x$species, "Sorbus aucuparia")
  it <- FruitTrait:::.fr_items(x)
  expect_true(all(c("photo_id", "path", "photo_url") %in% names(it)))
  expect_true(is.na(it$photo_url))
  unlink(d, recursive = TRUE)
})

test_that("species names are matched to the GBIF backbone", {
  fake <- function(url, query) {
    switch(query$name,
           "Sorbus aucuparia" = list(canonicalName = "Sorbus aucuparia", rank = "SPECIES", class = "Magnoliopsida",
                                     order = "Rosales", family = "Rosaceae", matchType = "EXACT"),
           "Apis mellifera" = list(canonicalName = "Apis mellifera", rank = "SPECIES", class = "Insecta",
                                   order = "Hymenoptera", family = "Apidae", matchType = "EXACT"),
           list(matchType = "NONE"))
  }
  local_mocked_bindings(.fr_get_json = fake)
  t1 <- fr_taxonomy("Sorbus aucuparia")
  expect_equal(t1$family, "Rosaceae")
  expect_true(t1$in_scope)
  expect_warning(t2 <- fr_taxonomy(c("Sorbus aucuparia", "Apis mellifera", "Nonexistent name")),
                 "Not matched to a flowering plant")
  expect_equal(nrow(t2), 3)
  expect_equal(t2$in_scope, c(TRUE, FALSE, FALSE))
  df <- data.frame(species = c("Sorbus aucuparia", "Sorbus aucuparia"), family = c("", "Rosaceae"),
                   stringsAsFactors = FALSE)
  out <- fr_taxonomy(df)
  expect_equal(out$family, c("Rosaceae", "Rosaceae"))
  expect_equal(out$order, c("Rosales", "Rosales"))
  expect_true(all(out$in_scope))
  expect_error(fr_taxonomy(data.frame(x = 1)), "species")
})

test_that("live GBIF and iNaturalist searches return fruiting observations", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("api.gbif.org")
  p <- fr_gbif_photos("Sorbus aucuparia", n = 2)
  expect_lte(nrow(p), 2)
  expect_true(all(p$license %in% c("CC0_1_0", "CC_BY_4_0")))
  q <- fr_inat_photos("Sorbus aucuparia", n = 2)
  expect_lte(nrow(q), 2)
  expect_true(all(q$license %in% c("cc0", "cc-by")))
  expect_equal(fr_taxonomy("Sorbus aucuparia")$family, "Rosaceae")
})

test_that("photos are downloaded once and failed downloads are reported", {
  ex <- fr_example_photos()
  d <- file.path(tempdir(), "fr_download_test")
  on.exit(unlink(d, recursive = TRUE))
  x <- data.frame(photo_id = c("a", "b"),
                  url = c(paste0("file://", normalizePath(ex$photo[1])), "file:///nonexistent/folder/x.jpg"),
                  stringsAsFactors = FALSE)
  expect_warning(y <- fr_download_photos(x, d), "could not be downloaded")
  expect_true(file.exists(y$path[1]))
  expect_true(is.na(y$path[2]))
  expect_equal(unname(tools::md5sum(y$path[1])), unname(tools::md5sum(ex$photo[1])))
  expect_error(fr_download_photos(data.frame(id = 1), d), "photo_id")
})
