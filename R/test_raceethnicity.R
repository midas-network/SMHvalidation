#' Runs Validation Checks on the `race_ethnicity` column
#'
#' Validate Scenario Modeling Hub submissions: test if the `race_ethnicity`
#' column contains the expected  value.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function contains 2 tests:
#'\itemize{
#'  \item{Race ethnicity value: }{If the submission contains projection by
#'  race/ethnicity, the `race_ethnicity` column contains the expected values as
#'  specified.}
#'  \item{Race ethnicity tasks id: }{If the submission contains projection by
#'  race/ethnicity and if one or multiple tasks ids required specific
#'  `race_ethnicity` value(s), no additional value(s) is provided in the
#'  submission file.}
#' }
#' Function called in the `validate_submission()` function, only if the
#' submission contains `"race_ethnicity"` column.
#'
#'@importFrom stats na.omit
#'@export
test_raceethnicity <- function(df, model_task) {

  test_raceethn <- lapply(model_task, function(x) {
    # Prerequisite
    req_raceethn <- x$task_ids$race_ethnicity$required
    opt_raceethn <- unique(x$task_ids$race_ethnicity$optional)
    all_raceethn <- unique(c(req_raceethn, opt_raceethn))
    df_test <- filter_df(df, x$task_ids, exclusion = "race_ethnicity")
    df_test <- data.table::data.table(df_test)

    if (dim(df_test)[1] > 0) {
      raceethn_vect <- unique(df_test$race_ethnicity)
      if (isFALSE(all(raceethn_vect %in% all_raceethn))) {
        raceethn_all <-
          paste0("\U000274c Error 1001: The `race_ethnicity` column contains ",
                 "unexpected value: '",
                 paste(raceethn_vect[!(raceethn_vect %in% all_raceethn)],
                       collapse = "', '"), "'. 'race_ethnicity' can only be: ",
                 paste(all_raceethn, collapse = ", "), ".")
      } else {
        raceethn_all <- NA
      }
      if (isFALSE(all(req_raceethn %in% raceethn_vect))) {
        raceethn_req <-
          paste0("\U000274c Error 1002: The `race_ethnicity` column is ",
                 "missing at least 1 required value: '",
                 paste(req_raceethn[!(req_raceethn %in% raceethn_vect)],
                       collapse = "', '"),
                 "'. 'race_ethnicity' should contain: ",
                 paste(req_raceethn, collapse = ", "), ".")
      } else {
        raceethn_req <- NA
      }
      test_raceethn <- unique(na.omit(c(raceethn_all, raceethn_req)))
    } else {
      test_raceethn <- NA # nocov
    }
    return(test_raceethn)
  })

  test_raceethn <- unique(na.omit(unlist(test_raceethn)))
  if (length(test_raceethn) == 0)
    test_raceethn <- "No errors or warnings found on `race_ethnicity`"

  return(test_raceethn)
}
