# nocov start
#' Runs Validation Checks on the Quantiles type and value columns
#'
#' **DEPRECATED** <br><br>
#' Validate Scenario Modeling Hub submissions: test if the
#' `quantile` and `type` columns contain the expected information and value
#' and the projection value increases with the quantiles.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function tests:
#' * Type: The submission contains quantiles value when expected.
#' * Quantiles: The `quantile` column matches the expected quantiles.
#' * Required quantiles: The projection should contain all required
#'   quantiles. It is accepted to submit less quantiles but the
#'   function will return a warning and the submission will not be included
#'   in the Ensembles.
#' * Value: The `value` associated with each `quantile` is increasing as
#'  the quantile increased. For example, if quantile 0.01 = 5 than quantile
#'  0.5 should be equal or greater than 5.
#' * Target: All targets required to have quantiles information have all
#'  required quantiles for each group id (scenario/location/horizon/etc.).
#'
#' `model_task` should match a specific round model tasks from the
#' `tasks.json` associated with the hub. The json is expected to follow
#' the [hubverse](https://hubverse.io/en/latest/user-guide/hub-config.html)
#' schema at least version 5.0
#'
#' As the function was *deprecated*, it will not be updated anymore. It was
#' updated a last time to match the 5.0 hubverse schema version. However, it
#' might returns duplicated message output or false error as for example
#' the 5.0 version of the `tasks.json` does not accept both required and
#' optional quantile for the same task ids and past SMH rounds might contains
#' required and optional quantiles for optional and/or required target.
#'
#'@importFrom tidyr all_of unite
#'@importFrom dplyr filter mutate distinct everything arrange lag
#'
#'@export
test_quantiles <- function(df, model_task) {

  warning("Function deprecated")

  lapply(model_task, function(x) {
    if ("quantile" %in% names(x$output_type)) {
      # Prerequisite
      if (x$output_type$quantile$is_required) {
        req_quantile <- x$output_type$quantile$output_type_id$required
        opt_quantile <- NULL
      } else {
        opt_quantile <- x$output_type$quantile$output_type_id$required
        req_quantile <- NULL
      }
      all_quantile <- unique(c(req_quantile, opt_quantile))
      df_test <- dplyr::filter(df, .data[["output_type"]] == "quantile" &
                                 .data[["target"]] %in%
                                   unique(unlist(x$task_ids$target)))
      if (dim(df_test)[1] > 0) {
        sub_quantile <- unlist(distinct(df_test[, "output_type_id"]))
        sub_quantile <- as.numeric(sub_quantile)
        # - test all quantiles value are expected
        if (isFALSE(all(all_quantile %in% sub_quantile))) {
          if (!all(req_quantile %in% sub_quantile)) {
            message("\U000274c Error: At least one quantile is missing: ",
                    paste(req_quantile[!req_quantile %in% sub_quantile],
                          collapse = ", "), ". Please verify")
          }
        }
        # - test submission does not have additional quantiles
        if (isFALSE(all(sub_quantile %in% all_quantile))) {
          message("\U000274c Error: At least one additional quantile was ",
                  "provided in the submission: ",
                  paste(sub_quantile[!sub_quantile %in% all_quantile],
                        collapse = ", "), ". Please verify")
        }
        # target(s) contains all the required quantiles
        sel_group <- names(x$task_ids)
        df_test <-
          dplyr::mutate(df_test,
                        output_type_id = as.numeric(.data[["output_type_id"]]),
                        sel = ifelse(all(req_quantile %in%
                                           .data[["output_type_id"]]),
                                     0, 1), .by = tidyr::all_of(sel_group))
        df_test2 <- dplyr::filter(df_test, .data[["sel"]] > 0)
        if (dim(df_test2)[1] > 0) {
          err_groups <-
            dplyr::select(df_test2,
                          -tidyr::all_of(c("sel", "value", "output_type",
                                           "output_type_id"))) |>
            dplyr::distinct() |>
            tidyr::unite("group", dplyr::everything(), sep = ", ") |>
            unlist()
          if (length(err_groups) > 25) err_groups <- err_groups[1:25]
          message("\U000274c Error: At least one quantile is missing from",
                  " the submission.", "The file will be accepted but might ",
                  "not be included in the Ensembles, please verify: ",
                  err_groups)
        }
        # - value increases with the quantiles
        df_qval <-
          dplyr::mutate(df_test,
                        output_type_id = as.numeric(.data[["output_type_id"]]))
        df_qval <- dplyr::arrange(df_qval, .data[["output_type_id"]]) |>
          dplyr::select(-tidyr::all_of(c("sel"))) |>
          dplyr::mutate(diff = .data[["value"]] - dplyr::lag(.data[["value"]]),
                        .by = tidyr::all_of(sel_group)) |>
          dplyr::filter(.data[["diff"]] < 0)
        if (dim(df_qval)[1] > 0) {
          err_groups <- df_qval |>
            dplyr::select(-tidyr::all_of(c("diff", "value", "output_type",
                                           "output_type_id"))) |>
            dplyr::distinct() |>
            tidyr::unite("group", dplyr::everything(), sep = ", ") |>
            unlist()
          if (length(err_groups) > 25) err_groups <- err_groups[1:25]
          message("\U000274c Error: Quantiles values are not increasing",
                  " with quantiles, please verify: ", err_groups)
        }
      } else {
        if (!is.null(x$task_ids$target$required) &
              x$output_type$quantile$is_required) {
          message("\U000274c Error: Quantiles are expected in the ",
                  "submission for the target(s): ",
                  paste(unique(unlist(x$task_ids$target$required)),
                        collapse = ", "), ". please verify.")
        }
      }
    }
    invisible(NULL)
  })
  invisible(NULL)
}
# nocov end
