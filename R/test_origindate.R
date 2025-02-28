
#' Runs Validation Checks on the Origin Date column
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `origin` column contains one unique date value corresponding
#' to the expected value, in the expected format.
#'
#'@param df data frame to test
#'@param path character vector path of the file being tested
#'@param id character date "id" of the corresponding round
#'
#'@details  This function contains 3 tests:
#' * Unique value: The `origin_date` column contains an unique expected
#'  value.
#' * Format: The `origin_date` column contains a unique date value in either a
#'  character ("YYYY-MM-DD") or a Date format. If the date is in a datetime
#'  format, a warning will be returned
#'
#' Function called in the `validate_submission()` function.
#'
#'@export
test_origindate <- function(df, path, id) {

  warning("Function deprecated")
  # Prerequisite
  vector_date <- unique(unlist(df[, "origin_date", TRUE]))
  # Test the format of the column: should be an unique value
  if (isFALSE(length(vector_date) == 1)) {
    message("\U000274c Error: 'origin_date' should contains 1 unique date. ",
            "The file contains multiple `origin_date' values: '",
            paste(vector_date, collapse = ", "), "'.")
  }

  # Test the column origin_date in not in Datetime format
  if (any(c("POSIXt", "POSIXct", "POSIXlt") %in% class(vector_date))) {
    message("\U0001f7e1 Warning: 'origin_date' is in the datetime format ",
            "(include time, timezone). To avoid issue please use a character ",
            "(`YYYY-MM-DD`) or a date format.")
  }

  # The origin_date value should correspond to the name of the file
  if (isFALSE(all(as.Date(id) == as.Date(vector_date)))) {
    message("\U000274c Error: 'origin_date' is not corresponding to the ",
            "expected value, the 'origin_date' date value should correspond ",
            "to: '", id, "'.")
  }
  invisible(NULL)
}
