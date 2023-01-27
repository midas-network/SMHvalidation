#' Runs Validation Checks on Columns names and number
#'
#' Validate Scenario Modeling Hub submissions: names and number of columns.
#'
#'@param df data frame to test
#'@param req_colname vector of required column names
#'
#'@details  This function contains 2 tests:
#'\itemize{
#'  \item{Name: }{The names of the columns are corresponding to the expected
#'  column names. If one column is misspelled or is missing, the validation
#'  will stop here with an error message and no other tests will be perform on
#'  the submission.}
#'  \item{Number: }{The submission should contains the expected number of
#'  columns.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'
test_column <- function(df, req_colnames) {
  # The name of the columns are corresponding to the expected format
  if (isFALSE(
    all(sort(names(df)) %in% sort(req_colnames)))) {
    fail_col <- sort(names(df))[!(sort(
      names(df)) %in% sort(req_colnames))]
    colnames_test <- paste0(
      "\U000274c Error 101: At least one column name is misspelled or does not",
      " correspond to the expected column names. The column(s) ",
      paste(fail_col, collapse = ", "),
      " do(es) not correspond to the standard")
  } else {
    colnames_test <- NA
  }
  # The number of the columns are corresponding to the expected format
  if (isFALSE(length(colnames(df)) == length(req_colnames))) {
    coldim_test <- paste0(
      "\U000274c Error 102: The data frame should contains ",
      length(req_colnames), " columns, not ",
      length(colnames(df)), ". Please verify if one or multiple columns have ",
      "been added or are missing.")
  } else {
    coldim_test <- NA
    if (!is.na(colnames_test)) {
      err_message3 <- paste0("\U000274c Error 103: ",
        "At least one column name is misspelled or does not correspond to ",
        "the expected column names. The column(s) ", paste(fail_col,
                                                           collapse = ", "),
        " do(es) not correspond to the standard. The rest of the validation ",
        "checks cannot be done if the columns names are not in the expected ",
        "standard format")
      cat(err_message3)
      stop("\n The submission contains one or multiple issues, please see ",
           "information above", call. = FALSE)
    }
  }

  col_test <- na.omit(c(colnames_test, coldim_test))
  col_test <- unique(col_test)
  if (length(col_test) == 0)
    col_test <- "No errors or warnings found on the column names and numbers"

  return(col_test)
}
