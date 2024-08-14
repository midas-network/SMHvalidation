#' Runs Validation Checks on the `age_group` column
#'
#' Validate Scenario Modeling Hub submissions: test if the `age_group` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function contains 3 tests:
#' * Age group: If the submission contains projection by age group,
#'  the `age_group` column contains the age group written `<AGEMIN>-<AGEMAX>`.
#' * Age group value: If the submission contains projection by
#'  age group, the `age_group` column contains the age group values as
#'  specify in the associated SMH GitHub Repository.
#' * Age group target: If the submission contains projection by
#'  age group and if one or multiple targets required specific `age_group`
#'  value(s), no additional value(s) is provided in the submission file.
#'
#' Function called in the `validate_submission()` function, only if the
#' submission contains `"age_group"` column
#'
#'@importFrom stats na.omit
#'@importFrom dplyr distinct select mutate %>%
#'@importFrom tidyr separate
#'@importFrom purrr discard map keep
#'@export
test_agegroup <- function(df, model_task) {

  test_age <- lapply(model_task, function(x) {
    # Prerequisite
    req_agegroup <- x$task_ids$age_group$required
    opt_agegroup <- unique(x$task_ids$age_group$optional)
    all_agegroup <- unique(c(req_agegroup, opt_agegroup))
    df_test <-
      data.table::data.table(df)[target %in%
                                 unique(unlist(x$task_ids$target)) &
                                 output_type %in% names(x$output_type)]

    if (dim(df_test)[1] > 0) {
      # - age group written `<AGEMIN>-<AGEMAX>`
      if (length(unique(grep("\\d{1,2}-\\d{1,3}", df_test$age_group,
                             value = TRUE, invert = TRUE))) > 0) {
        age_writ <-
          paste0("\U000274c Error 801: The `age_group` column should contain ",
                 "value written: 'AGEMIN-AGEMAX', one or more 'age_group' ",
                 "value(s) is not corresponding in the submission file, please",
                 " verify: '",
                 paste(unique(grep("\\d{1,2}-\\d{1,3}", df_test$age_group,
                                   value = TRUE, invert = TRUE)),
                       collapse = "', '"), "'.")
      } else {
        age_writ <- NA
      }

      age_vect <- unique(df_test$age_group)
      if (isFALSE(all(age_vect %in% all_agegroup))) {
        age_all <- paste0("\U000274c Error 802: The `age_group` column ",
                          "contains unexpected value: '",
                          paste(age_vect[!(age_vect %in% all_agegroup)],
                                collapse = "', '"),
                          "'. 'age_group' can only be: ",
                          paste(all_agegroup, collapse = ", "),
                          ", for the target: ",
                          paste(unique(unlist(x$task_ids$target)),
                                collapse = ", "), ".")
      } else {
        age_all <- NA
      }
      if (isFALSE(all(req_agegroup %in% age_vect))) {
        age_req <- paste0("\U000274c Error 802: The `age_group` column is ",
                          "missing at least 1 required value: '",
                          paste(req_agegroup[!(req_agegroup %in% age_vect)],
                                collapse = "', '"),
                          "'. 'age_group' should contain: ",
                          paste(req_agegroup, collapse = ", "),
                          ", for the target: ",
                          paste(unique(unlist(x$task_ids$target)),
                                collapse = ", "), ".")
      } else {
        age_req <- NA
      }
      test_age <- unique(na.omit(c(age_writ, age_all, age_req)))
    } else {
      test_age <- NA
    }
    return(test_age)
  })

  test_age <- unique(na.omit(unlist(test_age)))
  if (length(test_age) == 0)
    test_age <- "No errors or warnings found on Age_group"

  return(test_age)
}
