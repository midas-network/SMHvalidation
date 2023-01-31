
#' Runs Validation Checks on Scenario ID and Scenario Name columns
#'
#' Validate Scenario Modeling Hub submissions: test if the name and id of the
#' scenario correspond to the expected name and id and if they are correctly
#' associated with each others.
#'
#'@param df data frame to test
#'@param task_ids data.frame containing round information for each id columns
#'
#'@details  This function contains 1 tests:
#'\itemize{
#'  \item{Scenario ID: }{The IDs of the scenarios are correctly spelled and
#'  correspond to the scenario IDs of the corresponding round.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr select filter %>% distinct
#'@export
test_scenario <- function(df, task_ids) {
  # Prerequisite
  scenario_id <- unique(na.omit(unlist(task_ids$scenario_id)))

  # test scenario id are correctly spelled and correspond to the expected
  # values
  if (isFALSE(all(df %>% select(scenario_id) %>%  distinct() %>% unlist() %in%
                  scenario_id))) {
    scenid_test <-  paste0(
      "\U000274c Error 202: At least 1 of the 'scenario_id' do(es) not ",
      "correspond: '",
      paste(unique(df$scenario_id[!df$scenario_id %in% scenario_id]),
            collapse = ", "), "'. The scenarios ids for this round are: '",
      paste(scenario_id, collapse = ", "), "'. Please verify.")
  } else {
    scenid_test <- NA
  }

  scen_test <- na.omit(c(scenid_test))
  scen_test <- unique(scen_test)
  if (length(scen_test) == 0)
    scen_test  <- paste0("No errors or warnings found on scenario name and ",
                         "scenario id columns")

  return(scen_test)

}
