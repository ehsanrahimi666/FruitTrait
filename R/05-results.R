#' Read back-end results
#'
#' Reads `traits.csv` (one row per photo) and `fruits.csv` (one row per
#' detected fruit) written by [fr_process()] or by the command-line tool
#' `python -m fruittrait.run_photos`, adds the columns of `input.csv` when it is
#' present, and applies a decision rule with [fr_filter()].
#'
#' @param x Output folder of [fr_process()] or path to its `traits.csv`.
#' @inheritParams fr_filter
#' @return A data frame of class `fr_results`, one row per photo, with the
#'   back-end columns, `keep` and `reason`. The fruit table is stored in
#'   `attr(x, "fruits")` (see [fr_fruits()]) and the output folder in
#'   `attr(x, "dir")`. Main columns: `n_fruits_ok` (fruits that passed the
#'   outline checks), `m_n` and `m_L`, `m_a`, `m_b`, `m_C`, `m_hue`, `m_name`
#'   (number and mean colour of the measurable fruits), `v_*` (the same for all
#'   valid fruits, ripe or not), `m_wb_*` (grey-world white-balanced colour),
#'   `ring_*` (colour of the surroundings and its difference to the fruits) and
#'   quality metrics.
#' @examples
#' res <- fr_read_results(system.file("extdata", package = "FruitTrait"))
#' table(res$reason)
#' @export
fr_read_results <- function(x, rule = "measurable") {
  f <- if (dir.exists(x)) file.path(x, "traits.csv") else x
  if (!file.exists(f)) stop("No results file found: ", f, call. = FALSE)
  res <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
  inp <- file.path(dirname(f), "input.csv")
  if (file.exists(inp)) {
    meta <- utils::read.csv(inp, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
    add <- setdiff(names(meta), c(names(res), "path"))
    if (length(add)) res <- cbind(res, meta[match(res$photo_id, meta$photo_id), add, drop = FALSE])
  }
  for (k in intersect(c("mask_ok", "cut_by_frame", "measurable"), names(res))) res[[k]] <- .fr_lgl(res[[k]])
  ff <- file.path(dirname(f), "fruits.csv")
  fruits <- if (file.exists(ff)) utils::read.csv(ff, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
  else NULL
  if (!is.null(fruits)) {
    for (k in intersect(c("ok", "valid", "measurable", "dry"), names(fruits))) fruits[[k]] <- .fr_lgl(fruits[[k]])
  }
  if (rule %in% c("measurable", "valid") && !any(!is.na(res$measurable))) {
    message("No fruit-head results; using rule = \"outlined\".")
    rule <- "outlined"
  }
  res <- fr_filter(res, rule = rule)
  rownames(res) <- NULL
  attr(res, "fruits") <- fruits
  attr(res, "dir") <- dirname(f)
  res
}

#' Fruit table of results
#'
#' Returns the fruit table of a `fr_results` object (one row per detected
#' fruit), optionally only the fruits of the photos kept by the decision rule
#' and only fruits of a given status.
#'
#' @param x A `fr_results` data frame.
#' @param which `"all"` detected fruits, fruits that passed the outline checks
#'   (`"outlined"`), `"valid"` fruits (single fruit with a correct outline) or
#'   `"measurable"` fruits (valid and ripe).
#' @param kept_photos Only fruits of photos with `keep == TRUE`.
#' @return A data frame: `photo_id`, `fruit` (detection order), `label` (number
#'   in the label map), `ok`, `fail` (failed checks), box and outline in the
#'   original photo (`x0_photo` ... `outline_xy`), `px`, `fill`, `solidity`,
#'   `border`, `overlap`, `det`, `sam`, `diam_px_photo`, `elongation`, CIELAB
#'   colour (`L`, `a`, `b`, `C`, `hue_deg`, `name`), white-balanced colour
#'   (`wb_*`), `p_valid`, `p_ripe`, `p_dry` and the decisions `valid`,
#'   `measurable`, `dry`; with the photo columns `species` and `latitude` when
#'   present.
#' @examples
#' res <- fr_example_results()
#' fr_fruits(res, "outlined")[, c("photo_id", "label", "L", "a", "b", "name")]
#' @export
fr_fruits <- function(x, which = c("all", "outlined", "valid", "measurable"), kept_photos = FALSE) {
  which <- match.arg(which)
  f <- attr(x, "fruits")
  if (is.null(f)) stop("No fruit table: read the results with fr_read_results().", call. = FALSE)
  f <- switch(which, all = f, outlined = f[f$ok %in% TRUE, , drop = FALSE],
              valid = f[f$valid %in% TRUE, , drop = FALSE], measurable = f[f$measurable %in% TRUE, , drop = FALSE])
  if (kept_photos && "keep" %in% names(x)) f <- f[f$photo_id %in% x$photo_id[x$keep %in% TRUE], , drop = FALSE]
  for (k in intersect(c("species", "latitude", "longitude"), names(x))) {
    if (!k %in% names(f)) f[[k]] <- x[[k]][match(f$photo_id, x$photo_id)]
  }
  rownames(f) <- NULL
  f
}

#' Decision rules for photos
#'
#' Sets `keep` and `reason` for every photo:
#' \describe{
#'   \item{`"measurable"`}{(default) at least one measurable fruit: a single
#'     fruit that passed the outline checks, with `p_valid` and `p_ripe` of at
#'     least 0.5 (the rule of the data set);}
#'   \item{`"valid"`}{at least one valid fruit, ripe or not;}
#'   \item{`"outlined"`}{at least one fruit passed the outline checks (no fruit
#'     heads needed);}
#'   \item{`"none"`}{every photo that was processed without error.}
#' }
#'
#' @param x A data frame of back-end results.
#' @param rule One of the rules above.
#' @return `x` with `keep` and `reason`, of class `fr_results`.
#' @examples
#' res <- fr_example_results()
#' table(fr_filter(res, "outlined")$reason)
#' @export
fr_filter <- function(x, rule = c("measurable", "valid", "outlined", "none")) {
  rule <- match.arg(rule)
  status <- if ("status" %in% names(x)) x$status else if ("error" %in% names(x)) x$error else rep("ok", nrow(x))
  err <- !is.na(status) & nzchar(status) & status != "ok"
  outlined <- (x$mask_ok %||% rep(NA, nrow(x))) %in% TRUE
  meas <- (x$measurable %||% rep(NA, nrow(x))) %in% TRUE
  val <- suppressWarnings(as.numeric(x$v_n %||% rep(NA, nrow(x)))) >= 1
  keep <- switch(rule, measurable = meas, valid = val %in% TRUE, outlined = outlined, none = rep(TRUE, nrow(x)))
  reason <- ifelse(err, "error", ifelse(!outlined, "no single fruit outlined",
                   ifelse(rule == "measurable" & !meas & (val %in% TRUE), "no ripe fruit",
                          ifelse(rule %in% c("measurable", "valid") & !(val %in% TRUE), "no valid fruit", "kept"))))
  if (rule == "none") reason <- ifelse(err, "error", "kept")
  x$keep <- keep & !err
  x$reason <- reason
  class(x) <- unique(c("fr_results", class(x)))
  attr(x, "rule") <- rule
  x
}

#' @export
`[.fr_results` <- function(x, i, j, drop = FALSE) {
  a <- attributes(x)[c("fruits", "dir", "rule")]
  out <- NextMethod()
  if (is.data.frame(out)) {
    for (k in names(a)) attr(out, k) <- a[[k]]
    if (!is.null(a$fruits) && "photo_id" %in% names(out)) {
      attr(out, "fruits") <- a$fruits[a$fruits$photo_id %in% out$photo_id, , drop = FALSE]
    }
  }
  out
}

#' @export
print.fr_results <- function(x, ...) {
  n <- nrow(x)
  k <- sum(x$keep %in% TRUE)
  f <- attr(x, "fruits")
  cat("<fr_results>", n, "photos;", k, "kept by rule", shQuote(attr(x, "rule") %||% "?"), "\n")
  if (!is.null(f)) {
    cat(" fruits:", nrow(f), "detected,", sum(f$ok %in% TRUE), "outlined,", sum(f$valid %in% TRUE), "valid,",
        sum(f$measurable %in% TRUE), "measurable\n")
  }
  show <- intersect(c("photo_id", "species", "n_fruits_ok", "m_n", "m_L", "m_a", "m_b", "m_name", "keep", "reason"),
                    names(x))
  print(utils::head(as.data.frame(x)[, show, drop = FALSE], 10), row.names = FALSE)
  if (n > 10) cat(" ...", n - 10, "more photos\n")
  invisible(x)
}

#' @export
summary.fr_results <- function(object, ...) {
  out <- list(photos = nrow(object), kept = sum(object$keep %in% TRUE), rule = attr(object, "rule"),
              reasons = table(object$reason))
  f <- attr(object, "fruits")
  if (!is.null(f)) {
    out$fruits <- c(detected = nrow(f), outlined = sum(f$ok %in% TRUE), valid = sum(f$valid %in% TRUE),
                    measurable = sum(f$measurable %in% TRUE))
    fails <- unlist(strsplit(f$fail[!(f$ok %in% TRUE) & !is.na(f$fail)], ";"))
    out$failed_checks <- table(fails[nzchar(fails)])
    out$colour_names <- table(f$name[f$ok %in% TRUE & nzchar(f$name %||% "")])
  }
  class(out) <- "summary.fr_results"
  out
}

#' @export
print.summary.fr_results <- function(x, ...) {
  cat(x$photos, "photos;", x$kept, "kept by rule", shQuote(x$rule), "\n\nDecisions:\n")
  print(x$reasons)
  if (!is.null(x$fruits)) {
    cat("\nFruits:\n")
    print(x$fruits)
    if (length(x$failed_checks)) {
      cat("\nFailed outline checks:\n")
      print(x$failed_checks)
    }
    if (length(x$colour_names)) {
      cat("\nColour names of outlined fruits:\n")
      print(x$colour_names)
    }
  }
  invisible(x)
}

#' @export
plot.fr_results <- function(x, ...) {
  fr_plot_colours(x, ...)
}
