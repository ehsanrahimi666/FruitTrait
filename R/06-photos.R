#' Find fruit photos on iNaturalist
#'
#' Searches the iNaturalist API for observations of a plant taxon with photos
#' and returns one row per photo, with the licence and attribution given by
#' iNaturalist. Research-grade observations, photos under CC0 or CC BY and
#' observations annotated as fruiting ("Flowers and Fruits: Fruits or Seeds")
#' are returned by default. Requests are spaced by one second to respect the
#' API rate limit.
#'
#' @param taxon Scientific name or iNaturalist taxon id (a flowering plant).
#' @param n Maximum number of photos.
#' @param place_id Optional iNaturalist place id.
#' @param year Optional year of observation.
#' @param license Photo licences to keep (iNaturalist codes such as `"cc0"`,
#'   `"cc-by"`, `"cc-by-nc"`).
#' @param quality_grade `"research"` (default), `"needs_id"` or `"any"`.
#' @param fruiting `"annotated"` (default; annotation "Fruits or Seeds") or
#'   `"any"`.
#' @param max_per_observation Maximum photos per observation.
#' @return A data frame with `photo_id`, `observation_id`, `species`, `family`,
#'   `observed_on`, `latitude`, `longitude`, `place_guess`, `url` (original
#'   size), `license`, `rightsHolder`, `attribution` and `observation_url`.
#' @seealso [fr_download_photos()], [fr_gbif_photos()], [fr_in_scope()]
#' @examples
#' \dontrun{
#' ph <- fr_inat_photos("Sorbus aucuparia", n = 20)
#' ph <- fr_download_photos(ph, dir = "PATH_TO_PHOTO_FOLDER")
#' }
#' @export
fr_inat_photos <- function(taxon, n = 100, place_id = NULL, year = NULL, license = c("cc0", "cc-by"),
                           quality_grade = "research", fruiting = c("annotated", "any"), max_per_observation = 1) {
  fruiting <- match.arg(fruiting)
  tx <- .fr_inat_taxon(taxon)
  out <- list()
  got <- 0
  id_below <- NULL
  repeat {
    q <- list(taxon_id = tx$id, photos = "true", per_page = 200, order_by = "id", order = "desc",
              photo_license = paste(license, collapse = ","), place_id = place_id, year = year, id_below = id_below)
    if (quality_grade != "any") q$quality_grade <- quality_grade
    if (fruiting == "annotated") {
      q$term_id <- 12
      q$term_value_id <- 14
    }
    res <- .fr_get_json("https://api.inaturalist.org/v1/observations", q)$results
    if (!length(res)) break
    d <- .fr_parse_inat(res, license, max_per_observation)
    if (nrow(d)) {
      out[[length(out) + 1L]] <- d
      got <- got + nrow(d)
    }
    id_below <- min(vapply(res, function(r) as.numeric(r$id), 0))
    if (got >= n || length(res) < 200) break
    Sys.sleep(1)
  }
  res <- if (length(out)) do.call(rbind, out) else .fr_parse_inat(list(), license, 1)
  res <- utils::head(res, n)
  if (nrow(res)) {
    res$family <- .fr_inat_family(unique(res$taxon_id))[as.character(res$taxon_id)]
    sc <- fr_in_scope(unique(res$species))
    out_sc <- sc$species[!sc$in_scope]
    if (length(out_sc)) {
      warning("Not in the scope of FruitTrait (fleshy-fruited woody plants): ", paste(out_sc, collapse = ", "),
              call. = FALSE)
    }
  } else {
    res$family <- character()
  }
  rownames(res) <- NULL
  res[, c("photo_id", "observation_id", "species", "family", "observed_on", "latitude", "longitude",
          "place_guess", "url", "license", "rightsHolder", "attribution", "observation_url")]
}

#' Find fruit photos on GBIF
#'
#' Searches the GBIF occurrence API for photographed occurrences of a plant
#' taxon (by default research-grade 'iNaturalist' observations) and returns one
#' row per photo with its URL (original size), licence and rights holder. By
#' default only observations annotated on 'iNaturalist' as fruiting (reported by
#' GBIF as `reproductiveCondition` "fruits or seeds") are returned; GBIF cannot
#' filter on this field, so records are filtered after download.
#'
#' @param taxon Scientific name (matched with the GBIF backbone) or a GBIF taxon
#'   key.
#' @param n Maximum number of photos to return.
#' @param country Optional ISO 3166-1 alpha-2 country code(s).
#' @param year Optional year or range such as `"2020,2025"`.
#' @param license Photo licences to keep (GBIF codes `"CC0_1_0"`,
#'   `"CC_BY_4_0"`, `"CC_BY_NC_4_0"`).
#' @param fruiting `"annotated"` (default) or `"any"`.
#' @param dataset_key GBIF dataset key; the default is 'iNaturalist'
#'   research-grade observations. `NULL` searches all datasets.
#' @param max_per_occurrence Maximum photos per occurrence.
#' @param max_records Maximum number of occurrence records read from GBIF.
#' @return A data frame with `photo_id`, `occurrenceKey`, `species`, `family`,
#'   `country`, `year`, `eventDate`, `decimalLatitude`, `decimalLongitude`,
#'   `reproductiveCondition`, `url`, `license`, `rightsHolder` and `gbif_url`.
#' @seealso [fr_download_photos()], [fr_attribution()], [fr_inat_photos()]
#' @examples
#' \dontrun{
#' ph <- fr_gbif_photos("Sorbus aucuparia", n = 20, country = "DE")
#' }
#' @export
fr_gbif_photos <- function(taxon, n = 100, country = NULL, year = NULL, license = c("CC0_1_0", "CC_BY_4_0"),
                           fruiting = c("annotated", "any"), dataset_key = .fr_inat_key, max_per_occurrence = 1,
                           max_records = 3000) {
  fruiting <- match.arg(fruiting)
  m <- if (is.numeric(taxon)) .fr_get_json(paste0("https://api.gbif.org/v1/species/", taxon), list()) else
    .fr_get_json("https://api.gbif.org/v1/species/match", list(name = taxon))
  key <- .fr_check_plant(m)
  out <- list()
  got <- 0
  offset <- 0
  repeat {
    q <- c(list(taxonKey = key, mediaType = "StillImage", limit = 300, offset = offset, datasetKey = dataset_key,
                year = year),
           stats::setNames(as.list(country), rep("country", length(country))),
           stats::setNames(as.list(license), rep("license", length(license))))
    js <- .fr_get_json("https://api.gbif.org/v1/occurrence/search", q)
    res <- js$results
    rc <- vapply(res, function(r) if (is.null(r$reproductiveCondition)) "" else r$reproductiveCondition, "")
    keep <- if (fruiting == "any") rep(TRUE, length(res)) else grepl("fruits", rc, fixed = TRUE)
    d <- .fr_parse_occurrences(res[keep], license, max_per_occurrence)
    if (nrow(d)) {
      out[[length(out) + 1L]] <- d
      got <- got + nrow(d)
    }
    offset <- offset + length(res)
    if (got >= n || isTRUE(js$endOfRecords) || !length(res) || offset >= max_records) break
  }
  res <- if (length(out)) do.call(rbind, out) else .fr_parse_occurrences(list())
  res <- utils::head(res, n)
  rownames(res) <- NULL
  res
}

#' Download photos
#'
#' Downloads the photos listed by [fr_inat_photos()] or [fr_gbif_photos()] into
#' a folder, named by `photo_id`. Files already present are skipped.
#'
#' @param x Data frame with `photo_id` and `url` columns.
#' @param dir Destination folder.
#' @param size For 'iNaturalist' URLs: `"original"` (as in the data set; longer
#'   side up to 2048 px), `"large"` (1024 px) or `"medium"` (500 px). Smaller
#'   photos show fewer fruits large enough to be measured.
#' @param overwrite Download again if the file exists.
#' @return `x` with a `path` column (`NA` where the download failed).
#' @examples
#' \dontrun{
#' ph <- fr_inat_photos("Sorbus aucuparia", n = 5)
#' ph <- fr_download_photos(ph, dir = "PATH_TO_PHOTO_FOLDER")
#' }
#' @export
fr_download_photos <- function(x, dir, size = c("original", "large", "medium"), overwrite = FALSE) {
  size <- match.arg(size)
  if (!all(c("photo_id", "url") %in% names(x))) stop("`x` needs `photo_id` and `url` columns.", call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  url <- sub("/original\\.", paste0("/", size, "."), x$url)
  path <- file.path(dir, paste0(x$photo_id, ".jpg"))
  ok <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    if (file.exists(path[i]) && !overwrite) {
      ok[i] <- TRUE
      next
    }
    for (attempt in 1:2) {
      ok[i] <- tryCatch({
        curl::curl_download(url[i], path[i], handle = .fr_handle(), quiet = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (ok[i]) break
      Sys.sleep(1)
    }
    if (!ok[i] && file.exists(path[i])) unlink(path[i])
  }
  if (any(!ok)) warning(sum(!ok), " photo(s) could not be downloaded.", call. = FALSE)
  x$path <- ifelse(ok, normalizePath(path, mustWork = FALSE), NA_character_)
  x
}

#' Attribution lines for photos
#'
#' Builds the attribution that CC BY licences require when photos are shown,
#' for example in figures or supplementary material.
#'
#' @param x Data frame from [fr_inat_photos()], [fr_gbif_photos()] or
#'   [fr_example_photos()].
#' @return A character vector, one line per photo.
#' @examples
#' fr_attribution(fr_example_photos())
#' @export
fr_attribution <- function(x) {
  who <- x$rightsHolder %||% x$rights_holder
  lic <- .fr_license_code(x$license)
  lab <- c(CC0_1_0 = "CC0 1.0", CC_BY_4_0 = "CC BY 4.0", CC_BY_NC_4_0 = "CC BY-NC 4.0")[lic]
  lab[is.na(lab)] <- x$license[is.na(lab)]
  link <- x$observation_url %||% x$gbif_url %||% rep("", nrow(x))
  out <- paste0(x$species, ". Photo: ", who, " (", unname(lab), "), iNaturalist, ", link)
  if ("attribution" %in% names(x)) {
    inat <- !is.na(x$attribution) & nzchar(x$attribution)
    out[inat] <- paste0(x$species[inat], ". Photo: ", x$attribution[inat], ", iNaturalist, ", link[inat])
  }
  out
}

#' List the photos in a folder or in species folders
#'
#' Lists JPEG and PNG files in a folder and its sub-folders. When the photos are
#' sorted into one sub-folder per species (for example
#' `photos/Sorbus aucuparia/IMG_001.jpg`), the sub-folder name becomes the
#' species name. The table can be passed to [fr_process()].
#'
#' @param path Folder with photos, or with one sub-folder per species.
#' @param recursive Search sub-folders.
#' @param species_from_folder Take the species name from the name of the folder
#'   that holds each photo (underscores become spaces).
#' @return A data frame with `photo_id`, `path`, `species` and `file`.
#' @examples
#' d <- file.path(tempdir(), "species_photos")
#' ex <- fr_example_photos()
#' dir.create(file.path(d, ex$species[1]), recursive = TRUE, showWarnings = FALSE)
#' invisible(file.copy(ex$photo[1], file.path(d, ex$species[1], basename(ex$photo[1]))))
#' fr_read_folder(d)
#' @export
fr_read_folder <- function(path, recursive = TRUE, species_from_folder = TRUE) {
  if (!dir.exists(path)) stop("Folder not found: ", path, call. = FALSE)
  rel <- list.files(path, pattern = "\\.(jpe?g|png)$", ignore.case = TRUE, recursive = recursive)
  if (!length(rel)) stop("No images found in ", path, call. = FALSE)
  folder <- dirname(rel)
  species <- rep(NA_character_, length(rel))
  if (species_from_folder) {
    sub <- folder != "."
    species[sub] <- trimws(gsub("[_]+", " ", basename(folder[sub])))
  }
  id <- gsub("[^A-Za-z0-9._-]+", "_", tools::file_path_sans_ext(rel))
  id <- gsub("/", "__", id, fixed = TRUE)
  data.frame(photo_id = make.unique(id, sep = "_"), path = normalizePath(file.path(path, rel)), species = species,
             file = basename(rel), stringsAsFactors = FALSE)
}

#' Is a species in the scope of the method?
#'
#' Looks species up in [fr_scope], the fleshy-fruited woody species (trees,
#' shrubs and woody climbers) for which the method was built. Names are matched
#' exactly to accepted names of the World Checklist of Vascular Plants;
#' synonyms are not resolved.
#'
#' @param species Character vector of species names.
#' @return A data frame with `species`, `in_scope`, `family`, `woody_source`
#'   and `fleshy_source`.
#' @examples
#' fr_in_scope(c("Sorbus aucuparia", "Fragaria vesca", "Prunus avium"))
#' @export
fr_in_scope <- function(species) {
  sp <- trimws(as.character(species))
  sc <- .fr_data("fr_scope")
  i <- match(sp, sc$species)
  data.frame(species = sp, in_scope = !is.na(i), family = sc$family[i],
             woody_source = sc$woody_source[i], fleshy_source = sc$fleshy_source[i],
             stringsAsFactors = FALSE)
}

#' Taxonomy of species names from the GBIF backbone
#'
#' Matches species names to the GBIF backbone taxonomy and adds family, order
#' and class, and whether the species is in the scope of the method
#' ([fr_in_scope()]). Names that do not match a flowering plant raise a warning.
#' With a data frame (for example from [fr_read_folder()]), empty `family`,
#' `order` and `class` values are filled in.
#'
#' @param x Character vector of species names, or a data frame with a
#'   `species` column.
#' @return For a vector, a data frame with `species`, `matched_name`, `rank`,
#'   `class`, `order`, `family`, `match_type` and `in_scope`; for a data
#'   frame, `x` with `family`, `order`, `class` and `in_scope` added.
#' @examples
#' \donttest{
#' if (curl::has_internet()) fr_taxonomy(c("Sorbus aucuparia", "Viburnum opulus"))
#' }
#' @export
fr_taxonomy <- function(x) {
  nm <- if (is.data.frame(x)) x$species else x
  if (is.null(nm)) stop("`x` needs a `species` column.", call. = FALSE)
  u <- unique(stats::na.omit(trimws(as.character(nm))))
  rows <- lapply(u, function(s) {
    m <- tryCatch(.fr_get_json("https://api.gbif.org/v1/species/match", list(name = s)),
                  error = function(e) list())
    g <- function(k) if (is.null(m[[k]])) NA_character_ else as.character(m[[k]])
    data.frame(species = s, matched_name = g("canonicalName"), rank = g("rank"), class = g("class"),
               order = g("order"), family = g("family"), match_type = g("matchType"),
               stringsAsFactors = FALSE)
  })
  tab <- if (length(rows)) do.call(rbind, rows) else
    data.frame(species = character(), matched_name = character(), rank = character(),
               class = character(), order = character(), family = character(),
               match_type = character(), stringsAsFactors = FALSE)
  tab$in_scope <- fr_in_scope(tab$species)$in_scope
  bad <- tab$species[is.na(tab$class) | !tab$class %in% c("Magnoliopsida", "Liliopsida")]
  if (length(bad)) {
    warning("Not matched to a flowering plant (FruitTrait is built for fleshy fruits of woody plants): ",
            paste(bad, collapse = ", "), call. = FALSE)
  }
  if (!is.data.frame(x)) return(tab)
  i <- match(trimws(as.character(x$species)), tab$species)
  for (k in c("family", "order", "class")) {
    old <- if (k %in% names(x)) as.character(x[[k]]) else rep(NA_character_, nrow(x))
    x[[k]] <- ifelse(is.na(old) | old == "", tab[[k]][i], old)
  }
  x$in_scope <- tab$in_scope[i]
  x
}

.fr_get_json <- function(url, query) {
  query <- query[!vapply(query, is.null, logical(1))]
  qs <- paste(vapply(names(query), function(k) curl::curl_escape(k), ""),
              vapply(query, function(v) curl::curl_escape(as.character(v)), ""), sep = "=", collapse = "&")
  full <- if (length(query)) paste0(url, "?", qs) else url
  r <- NULL
  for (attempt in 1:3) {
    r <- tryCatch(curl::curl_fetch_memory(full, handle = .fr_handle()), error = function(e) e)
    ok <- !inherits(r, "error") && r$status_code == 200
    retry <- inherits(r, "error") || r$status_code == 429 || r$status_code >= 500
    if (ok || !retry) break
    Sys.sleep(2^(attempt - 1))
  }
  if (inherits(r, "error")) stop("The server could not be reached (", conditionMessage(r), "): ", full, call. = FALSE)
  if (r$status_code != 200) stop("Request failed (HTTP ", r$status_code, "): ", full, call. = FALSE)
  jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
}

.fr_handle <- function() {
  curl::new_handle(useragent = .fr_ua, connecttimeout = 30, timeout = 300)
}

.fr_check_plant <- function(m) {
  key <- m$usageKey %||% m$key
  if (is.null(key) || identical(m$matchType, "NONE")) stop("No GBIF backbone match for this name.", call. = FALSE)
  if (!(m$class %||% "") %in% c("Magnoliopsida", "Liliopsida")) {
    stop("'", m$scientificName, "' is not a flowering plant (class: ", m$class %||% "unknown", ").", call. = FALSE)
  }
  key
}

.fr_inat_taxon <- function(taxon) {
  if (is.numeric(taxon)) {
    t <- .fr_get_json(paste0("https://api.inaturalist.org/v1/taxa/", taxon), list())$results
  } else {
    t <- .fr_get_json("https://api.inaturalist.org/v1/taxa", list(q = taxon, per_page = 30))$results
    exact <- Filter(function(r) identical(tolower(r$name), tolower(trimws(taxon))), t)
    if (length(exact)) t <- exact
  }
  if (!length(t)) stop("No iNaturalist taxon found for '", taxon, "'.", call. = FALSE)
  t <- t[[1]]
  # 47125 = Angiospermae (flowering plants) on iNaturalist
  if (!(47125 %in% unlist(t$ancestor_ids) || identical(as.numeric(t$id), 47125))) {
    stop("'", t$name, "' is not a flowering plant on iNaturalist.", call. = FALSE)
  }
  list(id = t$id, name = t$name)
}

.fr_inat_family <- function(ids) {
  ids <- ids[!is.na(ids)]
  out <- character()
  for (chunk in split(ids, ceiling(seq_along(ids) / 30))) {
    res <- tryCatch(.fr_get_json(paste0("https://api.inaturalist.org/v1/taxa/", paste(chunk, collapse = ",")),
                                 list())$results, error = function(e) list())
    for (r in res) {
      fam <- Filter(function(a) identical(a$rank, "family"), c(r$ancestors, list(r)))
      out[as.character(r$id)] <- if (length(fam)) fam[[1]]$name else NA_character_
    }
  }
  out
}

.fr_parse_inat <- function(res, license = c("cc0", "cc-by"), max_per_observation = 1) {
  g <- function(x, k) if (is.null(x[[k]])) NA else x[[k]]
  rows <- list()
  for (r in res) {
    k <- 0
    for (ph in r$photos) {
      lic <- tolower(as.character(g(ph, "license_code")))
      if (is.na(lic) || !lic %in% license) next
      k <- k + 1
      if (k > max_per_observation) break
      loc <- suppressWarnings(as.numeric(strsplit(as.character(g(r, "location")), ",")[[1]]))
      who <- g(r$user, "name")
      if (is.na(who) || !nzchar(who)) who <- g(r$user, "login")
      rows[[length(rows) + 1L]] <- data.frame(
        photo_id = format(g(ph, "id"), scientific = FALSE), observation_id = format(r$id, scientific = FALSE),
        species = as.character(g(r$taxon, "name")), taxon_id = as.numeric(g(r$taxon, "id")),
        observed_on = as.character(g(r, "observed_on")),
        latitude = if (length(loc) == 2) loc[1] else NA_real_, longitude = if (length(loc) == 2) loc[2] else NA_real_,
        place_guess = as.character(g(r, "place_guess")), url = sub("/square\\.", "/original.", as.character(ph$url)),
        license = lic, rightsHolder = as.character(who), attribution = as.character(g(ph, "attribution")),
        observation_url = paste0("https://www.inaturalist.org/observations/", format(r$id, scientific = FALSE)),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(photo_id = character(), observation_id = character(), species = character(),
                      taxon_id = numeric(), observed_on = character(), latitude = numeric(), longitude = numeric(),
                      place_guess = character(), url = character(), license = character(),
                      rightsHolder = character(), attribution = character(), observation_url = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.fr_license_code <- function(u) {
  u <- tolower(trimws(ifelse(is.na(u), "", u)))
  ifelse(grepl("publicdomain/zero|cc0", u), "CC0_1_0",
  ifelse(grepl("licenses/by/|cc_by_4_0|^cc[- ]by( 4\\.0)?$", u), "CC_BY_4_0",
  ifelse(grepl("licenses/by-nc/|cc_by_nc_4_0|^cc[- ]by-nc( 4\\.0)?$", u), "CC_BY_NC_4_0", "other")))
}

.fr_parse_occurrences <- function(res, license = c("CC0_1_0", "CC_BY_4_0"), max_per_occurrence = 1) {
  g <- function(r, k) if (is.null(r[[k]])) NA else r[[k]]
  rows <- list()
  for (r in res) {
    media <- Filter(function(m) identical(m$type, "StillImage") && !is.null(m$identifier), r$media)
    k <- 0
    for (m in media) {
      lic <- .fr_license_code(g(m, "license"))
      if (!lic %in% license) next
      k <- k + 1
      if (k > max_per_occurrence) break
      who <- g(m, "rightsHolder")
      if (is.na(who)) who <- g(m, "creator")
      rows[[length(rows) + 1L]] <- data.frame(
        photo_id = sprintf("%s_%02d", format(g(r, "key"), scientific = FALSE), k),
        occurrenceKey = format(g(r, "key"), scientific = FALSE), species = g(r, "species"),
        family = g(r, "family"), country = g(r, "countryCode"), year = as.integer(g(r, "year")),
        eventDate = as.character(g(r, "eventDate")), decimalLatitude = as.numeric(g(r, "decimalLatitude")),
        decimalLongitude = as.numeric(g(r, "decimalLongitude")),
        reproductiveCondition = as.character(g(r, "reproductiveCondition")), url = m$identifier, license = lic,
        rightsHolder = who, gbif_url = paste0("https://www.gbif.org/occurrence/", format(g(r, "key"), scientific = FALSE)),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(photo_id = character(), occurrenceKey = character(), species = character(),
                      family = character(), country = character(), year = integer(), eventDate = character(),
                      decimalLatitude = numeric(), decimalLongitude = numeric(), reproductiveCondition = character(),
                      url = character(), license = character(), rightsHolder = character(), gbif_url = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}
