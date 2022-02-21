
#' Runs Validation Checks on Scenario ID and Scenario Name columns
#'
#' Validate Scenario Modeling Hub submissions: test if the name and id of the
#' scenario correspond to the expected name and id and if they are correctly
#' associated with each others.
#'
#'@param df data frame to test
#'@param round numeric corresponding to the current round number
#'@param scenario_smname named vector containing the scenario ID as name and the
#' corresponding abbreviated name as value (example: name: "A-2020-12-22",
#'  value: "optimistic")
#'@param scenario_sel named vector containing the round number as name in a
#' "roundX" format and the corresponding scenario ID as value (example:
#'  name: "round1", value: "A-2020-12-22")
#'
#'@details  This functions contains 3 tests:
#'\itemize{
#'  \item{"Scenario Name": }{The names of the scenarios are correctly spelled and
#'  correspond to the scenario name of the corresponding round.}
#'  \item{"Scenario ID": }{The IDs of the scenarios are correctly spelled and
#'  correspond to the scenario IDs of the corresponding round.}
#'  \item{"Correspondance": }{The names and ID of the scenarios are correctly
#'  matching (scenario ID A = scenario name A)}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@export
test_scenario <- function(df, round, scenario_smname, scenario_sel) {
  # test scenario name are correctly spelled and correspond to the expected
  # values
  if (isFALSE(all(df$scenario_name %in%
                  scenario_smname[scenario_sel[which(
                    names(scenario_sel) == paste0("round", round))]]))) {
    scenname_test <- paste0(
      "\U000274c Error: At least 1 of the 'scenario_name' do(es) not ",
      "correspond: '", unique(df$scenario_name[
        !df$scenario_name %in% scenario_smname[scenario_sel[which(
          names(scenario_sel) == paste0("round", round))]]]),
      "'. The scenarios names for this round are: '", paste(scenario_smname[
        scenario_sel[which(names(scenario_sel) ==  paste0("round", round))]],
        collapse = ", "), "'. Please verify.")
  } else {
    scenname_test <-NA
  }
  # test scenario id are correctly spelled and correspond to the expected
  # values
  if (isFALSE(all(df$scenario_id %in%scenario_sel[which(
    names(scenario_sel) == paste0("round", round))]))) {
    scenid_test <-  paste0(
      "\U000274c Error: At least 1 of the 'scenario_id' do(es) not ",
      "correspond: '", unique(df$scenario_id[!df$scenario_id %in%scenario_sel[
        which(names(scenario_sel) == paste0("round", round))]]),
      "'. The scenarios ids for this round are: '", paste(unique(scenario_sel[
        which(names(scenario_sel) == paste0("round", round))]),
        collapse = ", "), "'. Please verify.")
  } else {
    scenid_test <- NA
  }
  # test scenario name & scenario id are correctly linked together
  if (isFALSE(identical(df$scenario_name,
                        as.vector(scenario_smname[df$scenario_id])))) {
    scencorres_test <- paste0(
      "\U000274c Error: At least 1 of the 'scenario_id' do(es) not  correspond",
      " to the scenario name associated. Should be: \n'", paste(
        unique(names(scenario_smname[names(scenario_smname) %in% as.vector(
          scenario_sel[which(names(scenario_sel) == paste0("round", round))])])
        ), "=", unique(scenario_smname[names(scenario_smname) %in% as.vector(
          scenario_sel[which(names(scenario_sel) == paste0("round", round))])]
        ), collapse = "\n"), "',\n but is: \n\n'",
      paste(distinct(df[, c("scenario_id", "scenario_name")]) %>%
              tidyr::unite("name", sep = " = ") %>% unlist(), collapse = "\n"),
      "'.")
  } else {scencorres_test <- NA}

  scen_test <- na.omit(c(scenname_test, scenid_test, scencorres_test))
  if (length(scen_test) == 0)
    scen_test  <- paste0("No errors or warnings found on scenario name and ",
                         "scenario id columns")

  return(scen_test)

}
