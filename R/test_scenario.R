
#' Runs Validation Checks on Scenario ID and Scenario Name columns
#'
#' Validate Scenario Modeling Hub submissions: test if the name and id of the
#' scenario correspond to the expected name and id and if they are correctly
#' associated with each others.
#'
#'@param df data frame to test
#'@param js_def list containing round definitions: number and names of columns,
#' target names, ...
#'
#'@details  This function contains 3 tests:
#'\itemize{
#'  \item{Scenario Name: }{The names of the scenarios are correctly spelled and
#'  correspond to the scenario name of the corresponding round.}
#'  \item{Scenario ID: }{The IDs of the scenarios are correctly spelled and
#'  correspond to the scenario IDs of the corresponding round.}
#'  \item{Correspondance Name and ID: }{The names and ID of the scenarios are
#'  correctly matching (scenario ID A = scenario name A)}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr select filter %>% distinct
#'@importFrom purrr map
#'@importFrom tidyr unite
#'@export
test_scenario <- function(df, js_def) {
  # test scenario name are correctly spelled and correspond to the expected
  # values
  if (isFALSE(all(df$scenario_name %in% unlist(purrr::map(js_def$scenarios,
                                                          "name"))))) {
    scenname_test <- paste0(
      "\U000274c Error 201: At least 1 of the 'scenario_name' do(es) not ",
      "correspond: '", unique(df$scenario_name[
        !df$scenario_name %in% unlist(purrr::map(js_def$scenarios, "name"))]),
      "'. The scenarios names for this round are: '", paste(
        unlist(purrr::map(js_def$scenarios, "name")), collapse = ", "),
      "'. Please verify.")
  } else {
    scenname_test <-NA
  }
  # test scenario id are correctly spelled and correspond to the expected
  # values
  if (isFALSE(all(df$scenario_id %in% unlist(purrr::map(js_def$scenarios,
                                                        "id"))))) {
    scenid_test <-  paste0(
      "\U000274c Error 202: At least 1 of the 'scenario_id' do(es) not ",
      "correspond: '", unique(df$scenario_id[!df$scenario_id %in%
          unlist(purrr::map(js_def$scenarios, "id"))]),
      "'. The scenarios ids for this round are: '", paste(unlist(
        purrr::map(js_def$scenarios, "id")), collapse = ", "),
      "'. Please verify.")
  } else {
    scenid_test <- NA
  }
  # test scenario name & scenario id are correctly linked together
  scencorres_test <- lapply(js_def$scenarios, function(x) {
    test <- unlist(dplyr::select(dplyr::filter(df, scenario_id == x$id),
                                 scenario_name))
    test_res <- identical(unique(test), x$name)
    if (isFALSE(test_res)) {
      scencorres_test <-  paste0(
        "\U000274c Error 203: At least 1 of the 'scenario_id' do(es) not ",
        "correspond to the scenario name associated. Should be: \n'",
        paste(x, collapse = " = "), "'\n but is: \n'",
        paste(dplyr::distinct(df[, c("scenario_id", "scenario_name")]) %>%
                tidyr::unite("name", sep = " = ") %>% unlist(),
              collapse = "\n"),"'.")
    } else {
      scencorres_test <- NA
    }
    return(scencorres_test)
  })
  scencorres_test <- unique(unlist(scencorres_test))

  scen_test <- na.omit(c(scenname_test, scenid_test, scencorres_test))
  scen_test <- unique(scen_test)
  if (length(scen_test) == 0)
    scen_test  <- paste0("No errors or warnings found on scenario name and ",
                         "scenario id columns")

  return(scen_test)

}
