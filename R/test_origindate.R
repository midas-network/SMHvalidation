
#' Runs Validation Checks on the Origin Date column
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `origin` column contains one unique date value corresponding
#' to the projection start date of the round and also corresponds to the date
#' in the name of the submission file.
#'
#'@param df data frame to test
#'@param path character vector path of the file being tested
#'@param id character date "id" of the corresponding round
#'
#'@details  This function contains 3 tests:
#'\itemize{
#'  \item{Unique value: }{The `origin_date` column contains a unique
#'  value.}
#'  \item{Correspondance to the name of the file: }{The `origin_date`
#'  column contains a unique date value matching the date in the name of the
#'  submission file}
#'  \tiem{Format: }{The `origin_date`
#'  column contains a unique date value in either a character ("YYYY-MM-DD") or
#'  a Date format. If the date is in a datetime format, a warning will be
#'  returned}
#'}
#'
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom stringr str_extract
#'@export
test_origindate <- function(df, path, id) {
  # Prerequisite
  vector_date <- unique(unlist(df[, "origin_date", TRUE]))
  # Test the format of the column: should be an unique value
  if (isFALSE(length(vector_date) == 1)) {
    ordone_test <-
      paste0("\U000274c Error 302: 'origin_date' should contains 1 unique ",
             "date. The file contains multiple `origin_date' values: '",
             paste(vector_date, collapse = ", "), "'.")
  } else {
    ordone_test <- NA
  }

  # Test the column origin_date in not in Datetime format
  if (any(c("POSIXt", "POSIXct", "POSIXlt") %in% class(vector_date))) {
    ord_warn <-
      paste0("\U0001f7e1 Warning 305: 'origin_date' is in the datetime format ",
             "(include time, timezone). To avoid issue please use a character ",
             "(`YYYY-MM-DD`) or a date format.")
  } else {
    ord_warn <- NA
  }

  # The origin_date value should correspond to the name of the file
  date_pttr <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
  if (isFALSE(all(as.Date(stringr::str_extract(basename(path), date_pttr)) ==
                    vector_date))) {
    ordname_test <-
      paste0("\U000274c Error 303: 'origin_date' is not corresponding to ",
             "the name in the file, the 'origin_date' date value and the ",
             "date in the filename should correspond to: '", id, "'.")
  } else {
    ordname_test <- NA
  }

  ord_test <- na.omit(c(ordone_test, ordname_test, ord_warn))
  ord_test <- unique(ord_test)
  if (length(ord_test) == 0)
    ord_test  <- paste0("No errors or warnings found on the column ",
                        "'origin_date'")
  return(ord_test)
}
