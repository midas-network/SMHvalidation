
#' Runs Validation Checks on the Model Projection Date column
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `model_projection_date` column contains one unique date value corresponding
#' to the projection start date of the round and also corresponds to the date
#' in the name of the submission file.
#'
#'@param df data frame to test
#'@param path character vector path of the file being tested
#'@param start_date corresponds to the "1 wk ahead" target in the projection
#'  file
#'
#'@details  This function contains 4 tests:
#'\itemize{
#'  \item{Date value: }{The `model_projection_date` column contains date value}
#'  \item{Unique value: }{The `model_projection_date` column contains a unique
#'  value.}
#'  \item{Correspondance to the name of the file: }{The `model_projection_date`
#'  column contains a unique date value matching the date in the name of the
#'  submission file}
#'   \item{Correspondance to the projection starting date: }{The
#'  `model_projection_date` column contains a unique date value matching the
#'  projection starting date of the corresponding round}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom stringr str_extract
#'@export
test_modelprojdate <- function(df, path, start_date) {
  # Test the format of the column: should be a date
  if (any(is.na(as.Date(df$model_projection_date, "%Y-%m-%d")))) {
    mpddate_test <- paste0(
      "\U000274c Error 301: 'model_projection_date' should be a date, format ",
      "YYYY-MM-DD")
  } else {
    mpddate_test <- NA
  }

  # Test the format of the column: should be an unique value
  if (isFALSE(length(unique(df$model_projection_date)) == 1)) {
    mpdone_test <- paste0(
      "\U000274c Error 302: 'model_projection_date' should contains 1 unique ",
      "date. The file contains multiple `model_projection_date' values: '",
      paste(unique(df$model_projection_date), collapse = ", "), "'.")
  } else {
    mpdone_test <- NA
  }
  # The model_projection_date value should correspond to the name of the file
  if (isFALSE(as.Date(
    stringr::str_extract(basename(path),
                         "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")) ==
    unique(df$model_projection_date))) {
    mpdname_test <- paste0(
      "\U000274c Error 303: 'model_projection_date' is not corresponding to ",
      "the name in the file, the 'model_projection_date' date value and the ",
      "date in the filename should correspond to: '", start_date - 6, "'.")
  } else {
    mpdname_test <- NA
  }
  # The model_projection_date should correspond to the projection starting date
  if (isFALSE(unique(df$model_projection_date) == start_date - 6)) {
    mpdvalue_test <- paste0(
      "\U000274c Error 304: 'model_projection_date' should correspond to the ",
      "projection starting date, as written in the README available on GitHub.")
  } else {
    mpdvalue_test <- NA
  }

  mpd_test <- na.omit(c(mpddate_test, mpdone_test, mpdname_test, mpdvalue_test))
  mpd_test <- unique(mpd_test)
  if (length(mpd_test) == 0)
    mpd_test  <- paste0("No errors or warnings found on the column ",
                        "'model_projection_date'")

  return(mpd_test)
}
