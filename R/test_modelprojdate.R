
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
#'@details  This functions contains 4 tests:
#'\itemize{
#'  \item{"Date value": }{The `model_projection_date` column contains date value}
#'  \item{"Unique value": }{The `model_projection_date` column contains a unique
#'  value.}
#'  \item{"Correspond to the name of the file": }{The `model_projection_date`
#'  column contains a unique date value matching the date in the name of the
#'  submission file}
#'   \item{"correspond to the projection starting date": }{The
#'  `model_projection_date` column contains a unique date value matching the
#'  projection starting date of the corresponding round}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom lubridate is.Date
#'@importFrom stringr str_extract
#'@export
test_modelprojdate <- function(df, path, start_date) {
  # Test the format of the column: should be a date
  if (isFALSE(lubridate::is.Date(as.Date(df$model_projection_date)))) {
    mpddate_test <- paste0(
      "\U000274c Error: 'model_projection_date' should be a date, format ",
      "YYYY-MM-DD")
  } else {
    mpddate_test <- NA
  }
  # Test the format of the column: should be an unique value
  if (isFALSE(length(unique(df$model_projection_date)) == 1)) {
    mpdone_test <- paste0(
      "\U000274c Error: 'model_projection_date' should contains 1 unique date.",
      " The file contains multiple `model_projection_date' values: '",
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
      "\U000274c Error: 'model_projection_date' is not corresponding to the  ",
      "name in the file, the date should correspond to: '", paste(as.Date(
        stringr::str_extract(basename(path),
                             "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")),
        collapse = ", "), "'.")
  } else {
    mpdname_test <- NA
  }
  # The model_projection_date should correspond to the projection starting date
  if (isFALSE(unique(df$model_projection_date) == start_date - 6)) {
    mpdvalue_test <- paste0(
      "\U000274c Error: 'model_projection_date' should correspond to the ",
      "projection starting date, as written in the README available on GitHub.")
  } else {
    mpdvalue_test <- NA
  }

  mpd_test <- na.omit(c(mpddate_test, mpdone_test, mpdname_test, mpdvalue_test))
  if (length(mpd_test) == 0)
    mpd_test  <- paste0("No errors or warnings found on the column ",
                        "'model_projection_date'")

  return(mpd_test)
}
