#' Runs Validation Checks on Scenario ID and Scenario Name columns
#'
#' Validate Scenario Modeling Hub submissions: test if the name and id of the
#' scenario correspond to the expected name and id and if they are correctly
#' associated with each others.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function contains 2 tests:
#' * Scenario ID: The IDs of the scenarios are correctly spelled and
#'  correspond to the scenario IDs of the corresponding round.
#' * Required scenario: All the required scenario are present in the
#'  submission file (tested by target).
#'
#' Function called in the `validate_submission()` function.
#'
#'@importFrom dplyr select filter
#'@export
test_scenario <- function(df, model_task) {

  warning("Function deprecated")

  lapply(model_task, function(x) {
    # Prerequisite
    scenario_id <- unique(unlist(x$task_ids$scenario_id))
    req_scenario_id <- x$task_ids$scenario_id$required
    df_test <-
      dplyr::filter(df,
                    .data[["target"]] %in% unique(unlist(x$task_ids$target)) &
                      .data[["output_type"]] %in% names(x$output_type))
    if (nrow(df_test) < 1 & length(req_scenario_id) > 0) {
      df_req <- filter_df(df, x$task_ids, "scenario", required = TRUE)
      if (nrow(df_req) < 1) {
        text_val <- attr(df_req, "filter")
        if (any(grepl("target : ", text_val))) {
          type_err <- "\U000274c Error "
        } else {
          type_err <- "\U0001f7e1 Warning " # nocov
        }
        message(type_err,
                ": At least 1 of the required 'scenario_id' is missing.",
                " The required scenarios ids for the group: \n",
                paste(text_val, collapse = "\n"), ";\nare: '",
                paste(req_scenario_id, collapse = ", "), "'. Please verify.")
      }
    } else {
      lapply(unique(df_test$target), function(targ) {
        df_test_target <- dplyr::filter(df_test, .data[["target"]] == targ)
        vect_scen <- unique(df_test_target$scenario_id)
        # test scenario id are correctly spelled and correspond to the expected
        # values
        if (isFALSE(all(vect_scen %in% scenario_id))) {
          message("\U000274c Error: At least 1 of the 'scenario_id' do(es) not",
                  " correspond: '",
                  paste(unique(vect_scen[!vect_scen %in% scenario_id]),
                        collapse = ", "),
                  "'. The scenarios ids for this round are: '",
                  paste(scenario_id, collapse = ", "), "'. Please verify.")
        }
        if (isFALSE(all(req_scenario_id %in% vect_scen))) {
          message("\U000274c Error: At least 1 of the required ",
                  "'scenario_id' is missing: '",
                  paste(req_scenario_id[!req_scenario_id %in% vect_scen],
                        collapse = ", "),
                  "'. The scenarios ids for the target(s): ",
                  targ, " (", paste(names(x$output_type), collapse = ", "),
                  ") and for this round are: '",
                  paste(req_scenario_id, collapse = ", "), "'. Please verify.")
        }
        invisible(NULL)
      })
    }
    invisible(NULL)
  })
  invisible(NULL)
}
