# nocov start
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
#'* Target name: The target should correspond to the target name as
#'  expressed in the `model_task` parameter.
#'* Target number: The submission file contains projection for all
#'  required targets. It is accepted to submit only a subset of target but
#'  a warning and/or a error message will be return (depending if the target
#'  is optional or required).
#'* Number of week projected: The submission file contains projections
#'  for the expected number of weeks or more. If a team submits more week than
#'  expected, an error message will be returned. An error message will also be
#'  returned if the submission file contains less projection week than expected.
#'  Starting Feb. 2022, if the file
#'   contains less projected weeks than expected, the submission will still be
#'   accepted, but will return a warning message and might not be included
#'   in the Ensembles.
#'* Week projected: The submission file contains projection for all
#'  expected weeks for each tasks group (unique combination of `task_ids`
#'  columns as specified in the `model_task` parameter) and for all
#'  output type combination.
#'* NA target: The projection contains `NA` value in the `horizon`
#'  column for the target(s) requiring no time series information (for example,
#'  `"peak size hosp"`).
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr mutate filter select distinct everything
#'@importFrom tidyr all_of unite
#'@importFrom purrr map keep
#'@export
test_target <- function(df, model_task) {
  warning("Function deprecated")
  # - target names (should be the same as in the GitHub)
  target_names <- unique(unlist(purrr::map(purrr::map(model_task, "task_ids"),
                                           "target")))
  if (isFALSE(all(df$target %in% target_names))) {
    message("\U000274c Error: At least one of the target_names is ",
            "misspelled. Please verify, the target_names should be (optional ",
            "target(s) inluded): '", paste(target_names, collapse = ", "),
            "'. The data frame contains: '",
            paste(unique(df$target), collapse = ", "), "', as targets names.")
  }
  lapply(model_task, function(x) {
    # Prerequisite
    target_req <- x$task_ids$target$required
    req_horizon <- x$task_ids$horizon$required
    opt_horizon <- x$task_ids$horizon$optional
    df_target <-
      dplyr::filter(df,
                    .data[["target"]] %in% unique(unlist(x$task_ids$target)) &
                      .data[["output_type"]] %in% names(x$output_type))
    df_target <- loc_zero(df_target)
    if (dim(df_target)[1] > 0) {
      # - the submission contains all the targets. It is also accepted
      # to submit projections for only certain target (example: only cases,
      # etc.)
      df_req_target <- grep(paste(target_req, collapse = "|"),
                            unique(df_target$target), value = TRUE)
      if (length(df_req_target) < length(target_req)) {
        message("\U000274c Error: The data frame does not contain ",
                "projections for '",
                target_req[!target_req %in% df_req_target], "' target(s).")
      }
      # - Model projects for the expected number of weeks or more
      #  (if a team submit more we will still accept it)
      n_target_week <- length(req_horizon)
      max_week <- length(unique(c(req_horizon, opt_horizon)))
      if (isFALSE(length(unique(na.omit(df_target$horizon))) >=
                    n_target_week)) {
        message("\U0001f7e1 Warning: The projections should contain at least ",
                n_target_week, " weeks of projection. The data frame contains ",
                "only: ", length(unique(df_target$horizon)), " week(s). The",
                " projection might not be included in the Ensembles.")
      } else {
        if (isTRUE(length(unique(na.omit(df_target$horizon))) > max_week)) {
          message("\U000274c Error: The projection contains more projected ",
                  "week than expected. The projection should contain ",
                  max_week, " maximum.")
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
        df_ts <- dplyr::filter(df_target,
                               grepl(paste(target_ts, collapse = "|"),
                                     .data[["target"]]))
        sel_group <- grep(paste0("value|target_end_date|model_projection_date|",
                                 "scenario_name|horizon"),
                          names(df_ts), invert = TRUE, value = TRUE)
        df_ts <-
          dplyr::mutate(df_ts,
                        sel = ifelse(all(req_horizon %in% .data[["horizon"]]),
                                     0, 1), .by = tidyr::all_of(sel_group)) |>
          dplyr::filter(.data[["sel"]] > 0)
        if (dim(df_ts)[1] > 0) {
          err_groups <- df_ts |>
            dplyr::select(-tidyr::all_of(c("sel", "value"))) |>
            dplyr::distinct() |>
            tidyr::unite("group", dplyr::everything(), sep = ", ") |>
            unlist()
          err_groups <- err_groups[1:25]
          message("\U000274c Error: At least one target week is missing ",
                  "in the time series. Please verify: ", err_groups)
        }
      }
      # - all target weeks are NA by target, scenario, location, quantile/sample
      # for the target(s) requiring it
      if (length(target_pnt) > 0) {
        df_pnt <- dplyr::filter(df_target,
                                grepl(paste(target_pnt, collapse = "|"),
                                      .data[["target"]]))
        if (isFALSE(all(is.na(df_pnt$horizon)))) {
          df_pnt <- dplyr::filter(df_pnt, !is.na(.data[["horizon"]]))
          sel_group <- grep("value|model_projection_date|scenario_name",
                            names(df_pnt), invert = TRUE, value = TRUE)
          df_pnt <- dplyr::distinct(dplyr::select(df_pnt,
                                                  tidyr::all_of(sel_group)))
          err_groups <- df_pnt |>
            tidyr::unite("group", dplyr::everything(), sep = ", ") |>
            unlist()
          err_groups <- err_groups[1:25]
          message("\U000274c Error: The 'horizon' should be equal to NA for",
                  " the target(s): ", paste(target_pnt, collapse = ", "),
                  ". Please verify: ", err_groups)
        }
      }
    } else {
      missing_target <-
        paste(unlist(x$task_ids$target), " (",
              gsub("\\d$", "", names(unlist(x$task_ids$target))), ")", sep = "",
              collapse = ", ")
      if (any(grepl("required", missing_target))) {
        message("\U000274c Error: No value found associated with the targets: ",
                missing_target, "; output_type: ", paste(names(x$output_type),
                                                         collapse = ", "),
                ". Please verify.")
      } else {
        message("\U0001f7e1 Warning: No value found associated with the ",
                "targets: ", missing_target, "; output_type: ",
                paste(names(x$output_type), collapse = ", "), ".")
      }
    }
    invisible(NULL)
  })
  invisible(NULL)
}
# nocov end
