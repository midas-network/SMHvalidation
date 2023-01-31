
#' Runs Validation Checks on the Origin Date column
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `origin` column contains one unique date value corresponding
#' to the projection start date of the round and also corresponds to the date
#' in the name of the submission file.
#'
#'@param df data frame to test
#'@param path character vector path of the file being tested
#'
#'@details  This function contains 4 tests:
#'\itemize{
#'  \item{Date value: }{The `origin_date` column contains date value}
#'  \item{Unique value: }{The `origin_date` column contains a unique
#'  value.}
#'  \item{Correspondance to the name of the file: }{The `origin_date`
#'  column contains a unique date value matching the date in the name of the
#'  submission file}
#'   \item{Correspondance to the projection starting date: }{The
#'  `origin_date` column contains a unique date value matching the
#'  projection starting date of the corresponding round}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom stringr str_extract
#'@export
test_origindate <- function(df, path) {
  # Prerequisite
  vector_date <- df %>% select(origin_date) %>% distinct() %>% unlist()
  # Test the format of the column: should be a date
  if (any(is.na(as.Date(vector_date, "%Y-%m-%d")))) {
    ordate_test <- paste0(
      "\U000274c Error 301: 'origin_date' should be a date, format ",
      "YYYY-MM-DD")
  } else {
    ordate_test <- NA
  }
  # Test the format of the column: should be an unique value
  if (isFALSE(length(vector_date) == 1)) {
    ordone_test <- paste0(
      "\U000274c Error 302: 'origin_date' should contains 1 unique ",
      "date. The file contains multiple `origin_date' values: '",
      paste(vector_date, collapse = ", "), "'.")
  } else {
    ordone_test <- NA
  }
  # The origin_date value should correspond to the name of the file
  if (isFALSE(all(as.Date(
    stringr::str_extract(
      basename(path), "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")) ==
    vector_date))) {
    ordname_test <- paste0(
      "\U000274c Error 303: 'origin_date' is not corresponding to ",
      "the name in the file, the 'origin_date' date value and the ",
      "date in the filename should correspond to: '", vector_date, "'.")
  } else {
    ordname_test <- NA
  }

  ord_test <- na.omit(c(ordate_test, ordone_test, ordname_test))
  ord_test <- unique(ord_test)
  if (length(ord_test) == 0)
    ord_test  <- paste0("No errors or warnings found on the column ",
                        "'origin_date'")

  return(ord_test)
}
