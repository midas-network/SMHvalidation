#' Runs Validation Checks on the Target column
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `target` column contain the expected information and value.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function contains 5 tests:
#'\itemize{
#'  \item{Target name: }{The target should correspond to the target name as
#'  expressed in the `model_task` parameter. }
#'  \item{Target number: }{The submission file contains projection for all
#'  required targets. It is accepted to submit only a subset of target but
#'  a warning and/or a error message will be return (depending if the target
#'  is optional or required)}
#'  \item{Number of week projected: }{The submission file contains projections
#'  for the expected number of weeks or more. If a team submits more week than
#'  expected, a warning message will be returned, but the submission will be
#'  accepted. However, an error message will be returned if the submission file
#'  contains less projection week than expected. Starting Feb. 2022, if the file
#'   contains less projected weeks than expected, the submission will still be
#'   accepted, but will return a warning message and might not be included
#'   in the Ensembles}
#'  \item{Week projected: }{The submission file contains projection for all
#'  expected weeks for each tasks group (unique combination of `task_ids`
#'  columns as specified in the `model_task` parameter) and for all
#'  output type combination.}
#'  \item{NA target}{The projection contains `NA` value in the `horizon`
#'  column for the target(s) requiring no time series information (for example,
#'  `"peak size hosp"`)}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr %>% mutate filter
#'@importFrom purrr map keep
#'@export
test_target <- function(df, model_task) {

  # - target names (should be the same as in the GitHub)
  target_names <- unique(unlist(purrr::map(purrr::map(model_task, "task_ids"),
                                           "target")))
  if (isFALSE(all(df$target %in% target_names))) {
    targetname_test <-
      paste0("\U000274c Error 601: At least one of the target_names is ",
             "misspelled. Please verify, the target_names should be (optional ",
             "target(s) inluded): '", paste(target_names, collapse = ", "),
             "'. The data frame contains: '",
             paste(unique(df$target), collapse = ", "), "', as targets names.")
  } else {
    targetname_test <- NA
  }

  target_test <- lapply(model_task, function(x) {
    # Prerequisite
    target_req <- x$task_ids$target$required
    req_horizon <- x$task_ids$horizon$required
    opt_horizon <- x$task_ids$horizon$optional
    df_target <- data.table::data.table(df)
    df_target <- df_target[target %in% unique(unlist(x$task_ids$target)) &
                             output_type %in% names(x$output_type)]
    df_target <- loc_zero(df_target)
    if (dim(df_target)[1] > 0) {
      # - the submission contains all the targets. It is also accepted
      # to submit projections for only certain target (example: only cases,
      # etc.)
      df_req_target <- grep(paste(target_req, collapse = "|"),
                            unique(df_target$target), value = TRUE)
      if (length(df_req_target) < length(target_req)) {
        targetnum_test <-
          paste0("\U000274c Error 602: The data frame does not contain ",
                 "projections for '",
                 target_req[!target_req %in% df_req_target], "' target(s).")
      }  else {
        targetnum_test <- NA
      }
      # - Model projects for the expected number of weeks or more
      #  (if a team submit more we will still accept it)
      n_target_week <- length(req_horizon)
      max_week <- length(unique(c(req_horizon, opt_horizon)))
      if (isFALSE(length(unique(na.omit(df_target$horizon))) >=
                    n_target_week)) {
        targetweek_test <-
          paste0("\U0001f7e1 Warning 605: The projections should contains at ",
                 "least ", n_target_week,
                 " weeks of projection. The data frame contains only: ",
                 length(unique(df_target$horizon)), " week(s). The projection",
                 " might not be included in the Ensembles.")
      } else {
        if (isTRUE(length(unique(na.omit(df_target$horizon))) > max_week)) {
          targetweek_test <-
            paste0("\U0001f7e1 Warning 606: The projection contains more ",
                   "projected week than expected. The additional weeks might ",
                   "not be included in the Ensembles and/or visualizations")
        } else {
          targetweek_test <- NA
        }
      }
      # targets information for the horizon
      trg_meta <- x$target_metadata
      target_ts <-
        unlist(purrr::map(trg_meta[unlist(purrr::map(trg_meta,
                                                     "is_step_ahead"))],
                          "target_id"))
      target_pnt <-
        unlist(purrr::map(trg_meta[!unlist(purrr::map(trg_meta,
                                                      "is_step_ahead"))],
                          "target_id"))
      # - all target weeks are present (1,2,3, etc.) by target, scenario,
      # location, quantile or sample for the target(s) requiring it
      if (!is.null(target_ts)) {
        df_ts <- df_target[grepl(paste(target_ts, collapse = "|"), target)]
        sel_group <- grep(paste0("value|target_end_date|model_projection_date|",
                                 "scenario_name|horizon"),
                          names(df_ts), invert = TRUE, value = TRUE)
        df_ts[, sel := ifelse(all(req_horizon %in% horizon), 0, 1),
              by = sel_group]
        df_ts <- df_ts[sel > 0]
        if (dim(df_ts)[1] > 0) {
          err_groups <- df_ts %>%
            dplyr::select(-sel, -value) %>%
            dplyr::distinct() %>%
            tidyr::unite("group", dplyr::everything(), sep = ", ") %>%
            unlist()
          targetwnum_test <-
            paste0("\U000274c Error 607: At least one target week is missing ",
                   "in the time series. Please verify: ", err_groups)
        } else {
          targetwnum_test <- NA
        }
        if (length(na.omit(unlist(targetwnum_test))) > 100) {
          targetwnum_test <-
            paste0(unique(unlist(purrr::map(strsplit(targetwnum_test,
                                                     "Please verify: "), 1))),
                   length(targetwnum_test),
                   " unique groups have been identified with this issue. ",
                   "For example: \n",
                   paste("group: ", head(purrr::map(strsplit(targetwnum_test,
                                                             "verify: "), 2),
                                         3), collapse = ";\n"), "; \netc.")
        }
      } else {
        targetwnum_test <- NA
      }
      # - all target weeks are NA by target, scenario, location, quantile/sample
      # for the target(s) requiring it
      if (length(target_pnt) > 0) {
        df_pnt <- df_target[grepl(paste(target_pnt, collapse = "|"), target)]
        if (isFALSE(all(is.na(df_pnt$horizon)))) {
          df_pnt <- df_pnt[!is.na(horizon)]
          sel_group <- grep("value|model_projection_date|scenario_name",
                            names(df_pnt), invert = TRUE, value = TRUE)
          df_pnt <- dplyr::distinct(df_pnt[, ..sel_group])
          err_groups <- df_pnt %>%
            tidyr::unite("group", dplyr::everything(), sep = ", ") %>%
            unlist()
          targetwna_test <-
            paste0("\U000274c Error 612: The 'horizon' should be equal to NA",
                   " for the target(s): ", paste(target_pnt, collapse = ", "),
                   ". Please verify: ", err_groups)
        } else {
          targetwna_test <- NA
        }
      } else {
        targetwna_test <- NA
      }
      if (length(na.omit(unlist(targetwna_test))) > 100) {
        targetwna_test <-
          paste0(unique(unlist(purrr::map(strsplit(targetwna_test,
                                                   "Please verify: "), 1))),
                 length(targetwna_test), " unique groups have been identified ",
                 "with this issue. For example: \n",
                 paste("group: ", head(purrr::map(strsplit(targetwna_test,
                                                           "verify: "), 2),
                                       3), collapse = ";\n"), "; \netc.")
      }
      target_test <- na.omit(c(targetnum_test, targetweek_test, targetwnum_test,
                               targetwna_test))
    } else {
      missing_target <-
        paste(unlist(x$task_ids$target), " (",
              gsub("\\d$", "", names(unlist(x$task_ids$target))), ")", sep = "",
              collapse = ", ")
      if (any(grepl("required", missing_target))) {
        target_test <-
          paste0("\U000274c Error 602: No value found associated with the ",
                 "targets: ", missing_target, "; output_type: ",
                 paste(names(x$output_type), collapse = ", "),
                 ". Please verify.")
      } else {
        target_test <-
          paste0("\U0001f7e1 Warning 602: No value found associated with the ",
                 "targets: ", missing_target, "; output_type: ",
                 paste(names(x$output_type), collapse = ", "), ".")
      }
    }
    return(target_test)
  })
  target_test <- unique(na.omit(unlist(c(targetname_test, target_test))))
  if (length(target_test) == 0)
    target_test <- paste0("No errors or warnings found in target and ",
                          "associated columns")

  return(target_test)
}
