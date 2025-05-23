# nocov start
# Run all the age group specific column tests
#'@importFrom hubValidations capture_check_info capture_check_cnd
#'@importFrom dplyr filter
#'@importFrom purrr keep map
#'@noRd
check_age_group <- function(mtask, df, checks, file_path) {

  warning("Function Deprecated")

  # Prerequisite
  req_agegroup <- mtask$task_ids$age_group$required
  opt_agegroup <- unique(mtask$task_ids$age_group$optional)
  all_agegroup <- unique(c(req_agegroup, opt_agegroup))
  checks$info <-
    capture_check_info(file_path,
                       paste("Age group validation for {.var ",
                             paste(unique(unlist(mtask$task_ids$target)),
                                   collapse = ", "), "} target, {.var ",
                             paste(names(mtask$output_type)), "} output type"))
  df_test <-
    dplyr::filter(df, .data[["target"]] %in%
                    unique(unlist(mtask$task_ids$target)) &
                    .data[["output_type"]] %in% names(mtask$output_type))
  if (dim(df_test)[1] > 0) {
    # - age group written `<AGEMIN>-<AGEMAX>`
    format_checks <- !(length(grep("\\d{1,2}-\\d{1,3}", df_test$age_group,
                                   value = TRUE, invert = TRUE)) > 0)
    msg_dt <-
      paste0("The column should be written in a {.val <AGEMIN>-<AGEMAX>} ",
             "format.")
    checks$agegroup_format <-
      capture_check_cnd(format_checks, file_path,
                        msg_subject = "{.var age_group}", details = msg_dt,
                        msg_attribute = "in a valid format.", error = TRUE)
    # -- contains expected value
    age_vect <- unique(df_test$age_group)
    req_check <- all(age_vect %in% all_agegroup)
    msg_dt <- NULL
    if (isFALSE(req_check))
      msg_dt <- paste0("{.val ", paste(age_vect[!(age_vect %in% all_agegroup)],
                                       collapse = ", "),
                       "} not accepted value.")
    checks$expect_agegroup_value <-
      capture_check_cnd(req_check, file_path, error = TRUE,
                        msg_subject = "{.var age_group}",
                        msg_verbs = c("contains expected",
                                      "contains unexpected"),
                        msg_attribute = "value.", details = msg_dt)
    req_check <- all(req_agegroup %in% age_vect)
    msg_dt <- NULL
    if (isFALSE(req_check))
      msg_dt <- paste0("{.val ",
                       paste(age_vect[!(age_vect %in% all_agegroup)],
                             collapse = ", "), "} is nott required.")
    checks$require_agegroup_value <-
      capture_check_cnd(req_check, file_path,  error = TRUE,
                        msg_subject = "{.var age_group}",
                        msg_verbs = c("has all", "is missing"),
                        msg_attribute = "required values.", details = msg_dt)
  } else {
    checks <- NULL
  }
  checks
}


#' Runs Validation Checks on the `age_group` column
#'
#' **DEPRECATED** <br><br>
#' Validate Scenario Modeling Hub submissions: test if the `age_group` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.) for a specific round
#'@param file_name optional, name of the file tested
#'
#'@details  This function  tests:
#' * Age group: If the submission contains projection by age group,
#'  the `age_group` column contains the age group written `<AGEMIN>-<AGEMAX>`.
#' * Age group value: If the submission contains projection by
#'  age group, the `age_group` column contains the age group values as
#'  specify in the associated SMH GitHub Repository.
#' * Age group target: If the submission contains projection by
#'  age group and if one or multiple targets required specific `age_group`
#'  value(s), no additional value(s) is provided in the submission file.
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
#' @return NULL
#'
#'@importFrom hubValidations new_hub_validations
#'@importFrom purrr keep map
#'@export
test_agegroup <- function(df, model_task, file_name = "") {

  checks <- hubValidations::new_hub_validations()
  test_age <- purrr::map(model_task, check_age_group, df, checks, file_name)
  print(purrr::keep(test_age, ~ !is.null(.x)))
  invisible(NULL)
}
# nocov end
