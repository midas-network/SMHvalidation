
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
    df_test <- data.table::data.table(df)
    if (any(nchar(df_test$location) == 1)) {
      df_test$location[which(nchar(df_test$location) == 1)] <- paste0(
        0, df_test$location[which(nchar(df_test$location) == 1)])
    }
    scenario_id <- unique(unlist(x$scenario_id))
    req_scenario_id <- x$scenario_id$required
    # filter
    col_names <- grep("scenario", names(x), invert = TRUE, value = TRUE)
    filter_var <- setNames(lapply(col_names, function(y) unique(unlist(x[[y]]))), col_names)
    filter_var <- purrr::discard(filter_var, is.null)
    for (i in 1:length(filter_var)) {
      if (grepl("date", names(filter_var)[i])) {
        df_test[[names(filter_var)[i]]] <- as.Date(df_test[[names(filter_var)[i]]])
        filter_var[[names(filter_var)[i]]] <- as.Date(filter_var[[names(filter_var)[i]]])
      }
      df_test <- df_test[df_test[[names(filter_var)[i]]] %in% filter_var[[i]]]
    }
    if (nrow(df_test) < 1 & length(req_scenario_id) > 0) {
      if (!is.null(x$scenario_id$required)) {
        filter_var_req <- setNames(lapply(col_names, function(y) unique(unlist(x[[y]]$required))), col_names)
        filter_var_req <- purrr::discard(filter_var_req, is.null)
        df_test <- data.table::data.table(df)
        for (i in 1:length(filter_var_req)) {
          if (grepl("date", names(filter_var_req)[i])) {
            df_test[[names(filter_var_req)[i]]] <- as.Date(df_test[[names(filter_var_req)[i]]])
            filter_var_req[[names(filter_var_req)[i]]] <- as.Date(filter_var[[names(filter_var_req)[i]]])
          }
          df_test <- df_test[df_test[[names(filter_var_req)[i]]] %in% filter_var_req[[i]]]
        }
        if (nrow(df_test) < 1 & length(req_scenario_id) > 0) {
          text_val <- paste(
            names(filter_var_req), ": ",  purrr::map(
              filter_var_req,  function(x) {
                if (length(x) > 10) {
                  paste0(paste(na.omit(x[1:10]), collapse = ", "), ", ...")
                } else {
                  paste(na.omit(x[1:10]), collapse = ", ")
                }
              }))
          if (any(grepl("target", names(filter_var_req)))) {
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
