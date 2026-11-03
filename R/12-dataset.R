#' Read the tables of the FruitTraits data set
#'
#' Reads a table of the FruitTraits data set (photographs, fruits, species,
#' validation photographs or rated benchmark fruits) with the column types of
#' its data dictionary: only empty fields are missing values, so that the ISO
#' country code of Namibia (`"NA"`) is kept; `True`/`False` columns become
#' logical; identifiers (`photo_id`, `gbifID` and other columns ending in `id`,
#' `ID` or `Key`) stay character; other columns are converted to numbers when
#' all their values are numeric.
#'
#' @param dir Folder of the data set, or the path of one table.
#' @param table Table to read when `dir` is a folder: `"photos"`, `"fruits"`,
#'   `"species"`, `"validation_photos"` or `"benchmark_fruits"`.
#' @return A data frame.
#' @examples
#' f <- tempfile(fileext = ".csv")
#' writeLines(c("photo_id,countryCode,measurable,m_L",
#'              "123456789,NA,True,41.2", "987654321,US,False,"), f)
#' x <- fr_read_dataset(f)
#' str(x)
#' @export
fr_read_dataset <- function(dir, table = c("photos", "fruits", "species", "validation_photos",
                                           "benchmark_fruits")) {
  table <- match.arg(table)
  if (dir.exists(dir)) {
    cand <- file.path(dir, paste0(table, c(".csv.gz", ".csv")))
    f <- cand[file.exists(cand)][1]
    if (is.na(f)) stop("Table '", table, "' not found in ", dir, call. = FALSE)
  } else if (file.exists(dir)) {
    f <- dir
  } else {
    stop("Not found: ", dir, call. = FALSE)
  }
  x <- utils::read.csv(f, na.strings = "", colClasses = "character", check.names = FALSE,
                       encoding = "UTF-8", stringsAsFactors = FALSE)
  id <- grepl("(^|_)id$|ID$|Key$", names(x))
  for (k in names(x)[!id]) {
    v <- x[[k]]
    u <- unique(v[!is.na(v)])
    if (length(u) && all(u %in% c("True", "False", "TRUE", "FALSE", "true", "false"))) {
      x[[k]] <- toupper(v) == "TRUE"
    } else if (length(u) && !anyNA(suppressWarnings(as.numeric(u))) && !k %in% c("countryCode", "country")) {
      x[[k]] <- as.numeric(v)
    }
  }
  x
}
