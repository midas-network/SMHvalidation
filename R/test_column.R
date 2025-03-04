#' Runs Validation Checks on Columns names and number
#'
#' Validate Scenario Modeling Hub submissions: names and number of columns.
#'
#'@param df data frame to test
#'@param req_colnames vector of required column names
#'@param file_name optional, name of the file tested
#'
#'@details  This function contains 2 tests:
#' *Name: The names of the columns are corresponding to the expected
#'  column names. If one column is misspelled or is missing, the validation
#'  will stop here with an error message and no other tests will be perform on
#'  the submission.
#' *Number: The submission should contains the expected number of
#'  columns.
#'
#' Function called in the `validate_submission()` function.
#'
#'@importFrom hubValidations new_hub_validations capture_check_cnd
#'@export
test_column <- function(df, req_colnames, file_name = "") {

  warning("Function Deprecated")

  checks <- hubValidations::new_hub_validations()
  # The name of the columns are corresponding to the expected format
  format_checks <- (all(req_colnames %in% names(df)))
  msg_dt <- NULL
  if (isFALSE(format_checks))
    msg_dt <- paste0("{.val ",
                     paste(req_colnames[!req_colnames %in% names(df)],
                           collapse = ", "),
                     "} do(es) not correspond to the standard.")
  checks$columns_variable <-
    capture_check_cnd(format_checks, file_name, error = TRUE,
                      msg_subject = "Columns",
                      msg_verbs = c("are matching the expected",
                                    "contain unexpected"),
                      msg_attribute = "names.", details = msg_dt)
  # The number of the columns are corresponding to the expected format
  number_checks <- !(length(colnames(df)) != length(req_colnames))
  msg_dt <- NULL
  if (isFALSE(number_checks))
    msg_dt <- paste0("The data frame should contains ", length(req_colnames),
                     " columns, not ", length(colnames(df)))
  checks$columns_number <-
    capture_check_cnd(number_checks, file_name, error = TRUE,
                      msg_subject = "The submission ",
                      msg_verbs = c("has the expected number of",
                                    "miss or have an additional"),
                      msg_attribute = "column", details = msg_dt)
  print(checks)
  invisible(NULL)

}
