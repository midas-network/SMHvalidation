#' Runs Validation Checks on Scenario ID and Scenario Name columns
#'
#' Validate Scenario Modeling Hub submissions: test if the name and id of the
#' scenario correspond to the expected name and id and if they are correctly
#' associated with each others.
#'
#'@param df data frame to test
#'@param task_ids list containing round information for each id columns
#'
#'@details  This function contains 2 tests:
#'\itemize{
#'  \item{Scenario ID: }{The IDs of the scenarios are correctly spelled and
#'  correspond to the scenario IDs of the corresponding round.}
#'  \item{Required scenario: }{All the required scenario are present in the
#'  submission file (tested by target).}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr select filter %>% distinct
#'@export
test_scenario <- function(df, task_ids) {

  scen_test <- lapply(task_ids, function(x) {
    # Prerequisite
    scenario_id <- unique(unlist(x$scenario_id))
    req_scenario_id <- x$scenario_id$required
    df_test <- filter_df(df, x, "scenario")
    if (nrow(df_test) < 1 & length(req_scenario_id) > 0) {
      if (!is.null(x$scenario_id$required)) {
        df_req <- filter_df(df, x, "scenario", required = TRUE)
        if (nrow(df_req) < 1 & length(req_scenario_id) > 0) {
          text_val <- attr(df_req, "filter")
          if (any(grepl("target: ", names(text_val)))) {
            type_err <- "\U000274c Error "
          } else {
            type_err <- "\U0001f7e1 Warning "
          }
          scen_test <- paste0(
            type_err, "204: At least 1 of the required 'scenario_id' is ",
            "missing. The required scenarios ids for the ",
            "group: \n", paste(text_val, collapse = "\n"), "; are: '",
            paste(req_scenario_id, collapse = ", "),
            "'. Please verify.")
        } else {
          scen_test <- NA
        }
      } else {
        scen_test <- NA
      }
    } else {
      vect_scen <- distinct(select(df_test, scenario_id)) %>% unlist() %>%
        unique()
      # test scenario id are correctly spelled and correspond to the expected
      # values
      if (isFALSE(all(vect_scen %in% scenario_id))) {
        scenid_test <-  paste0(
          "\U000274c Error 202: At least 1 of the 'scenario_id' do(es) not ",
          "correspond: '", paste(unique(vect_scen[!vect_scen %in% scenario_id]),
                                 collapse = ", "), "'. The scenarios ids for ",
          "this round are: '", paste(scenario_id, collapse = ", "),
          "'. Please verify.")
      } else {
        scenid_test <- NA
      }
      if (isFALSE(all(req_scenario_id %in% vect_scen))) {
        scen_req_test <-  paste0(
          "\U000274c Error 204: At least 1 of the required 'scenario_id' is ",
          "missing: '", paste(req_scenario_id[!req_scenario_id %in% vect_scen],
                              collapse = ", "), "'. The scenarios ids for the ",
          "target(s): ",paste(unique(unlist(x$target)), collapse = ", "), " and ",
          "for this round are: '", paste(req_scenario_id, collapse = ", "),
          "'. Please verify.")
      } else {
        scen_req_test <- NA
      }
      scen_test <- na.omit(c(scenid_test, scen_req_test))
    }
    scen_test <- unique(scen_test)
    return(scen_test)
  })

  scen_test <- unique(na.omit(unlist(scen_test)))
  if (length(scen_test) == 0)
    scen_test  <- paste0("No errors or warnings found on scenario name and ",
                         "scenario id columns")

  return(scen_test)

}
