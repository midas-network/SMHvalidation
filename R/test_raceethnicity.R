# nocov start
#' Runs Validation Checks on the `race_ethnicity` column
#'
#' **DEPRECATED** <br><br>
#' Validate Scenario Modeling Hub submissions: test if the `race_ethnicity`
#' column contains the expected  value.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function tests:
#' * Race ethnicity value: If the submission contains projection by
#'  race/ethnicity, the `race_ethnicity` column contains the expected values as
#'  specified.
#' * Race ethnicity tasks id: If the submission contains projection by
#'  race/ethnicity and if one or multiple tasks ids required specific
#'  `race_ethnicity` value(s), no additional value(s) is provided in the
#'  submission file.
#'
#' `model_task` should match a specific round model tasks from the
#' `tasks.json` associated with the hub. The json is expected to follow
#' the [hubverse](https://hubverse.io/en/latest/user-guide/hub-config.html)
#' schema at least version 5.0
#'
#' As the function was deprecated, it will not be updated anymore. It was
#' updated a last time to match the 5.0 hubverse schema version. However, it
#' might returns duplicated message output.
#'
#'@export
test_raceethnicity <- function(df, model_task) {

  warning("Function deprecated")

  lapply(model_task, function(x) {
    # Prerequisite
    req_raceethn <- x$task_ids$race_ethnicity$required
    opt_raceethn <- unique(x$task_ids$race_ethnicity$optional)
    all_raceethn <- unique(c(req_raceethn, opt_raceethn))
    df_test <- filter_df(df, x$task_ids, exclusion = "race_ethnicity")

    if (dim(df_test)[1] > 0) {
      raceethn_vect <- unique(df_test$race_ethnicity)
      if (isFALSE(all(raceethn_vect %in% all_raceethn))) {
        message("\U000274c Error: The `race_ethnicity` column contains ",
                "unexpected value: '",
                paste(raceethn_vect[!(raceethn_vect %in% all_raceethn)],
                      collapse = "', '"), "'. 'race_ethnicity' can only be: ",
                paste(all_raceethn, collapse = ", "), ".")
      }
      if (isFALSE(all(req_raceethn %in% raceethn_vect))) {
        message("\U000274c Error: The `race_ethnicity` column is ",
                "missing at least 1 required value: '",
                paste(req_raceethn[!(req_raceethn %in% raceethn_vect)],
                      collapse = "', '"),
                "'. 'race_ethnicity' should contain: ",
                paste(req_raceethn, collapse = ", "), ".")
      }
    }
    invisible(NULL)
  })
  invisible(NULL)
}
# nocov end
