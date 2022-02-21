#' Run all validation checks and output a report
#'
#' Runs all the different validation checks functions (test_column,
#' test_scenario, test_modelprojdate, test_quantiles, test_val, test_target,
#' test_location) on a data frame and print information about the results of
#' each tests on the submission: warning(s), error(s) or if all the tests
#' were successful.
#'
#'@param df data frame to test
#'@param round numeric corresponding to the current round number
#'@param scenario_smname named vector containing the scenario ID as name and the
#' corresponding abbreviated name as value (example: name: "A-2020-12-22",
#'  value: "optimistic")
#'@param scenario_sel named vector containing the round number as name in a
#' "roundX" format and the corresponding scenario ID as value (example:
#'  name: "round1", value: "A-2020-12-22")
#'@param path character vector path of the file being tested
#'@param start_date corresponds to the "1 wk ahead" target in the projection
#'  file
#'@param pop data frame containing the population size of each geographical
#'  entities by fips (in a column "location")
#'@param number2location named vector containing the FIPS as name and the
#'  corresponding location name as value (example: name: "01", value: "Alabama")
#'@param last_lst_gs list of data frame, named with the corresponding target and
#'  containing the last avaible week of observed data  before start of the
#'  projection
#'
#'@details Internal function called in the `validation_submission()` function. For
#' more information on all tests run on the submission, please refer to the
#' documentation of each "test_*" function. A vignette with all the information
#' might be created later on too.
#'
#' @noRd
run_all_validation <- function(df, round, start_date, path, pop,
                               last_lst_gs, scenario_smname,
                               scenario_sel, number2location) {
  # Tests:
  out_col <- test_column(df)
  out_scen <- test_scenario(df, round, scenario_smname, scenario_sel)
  out_mpd <- test_modelprojdate(df, path, start_date)
  out_quant <- test_quantiles(df, round)
  out_val <- test_val(df, pop, last_lst_gs)
  out_target <- test_target(df, start_date, round)
  out_loc <- test_location(df, number2location)
  # Report:
  test_report <- paste(
    " ## Columns: \n\n", paste(out_col, collapse = "\n"), "\n\n",
    "## Scenarios: \n\n", paste(out_scen, collapse = "\n"), "\n\n",
    "## Model Projection Date Column:  \n\n", paste(out_mpd, collapse = "\n"),
    "\n\n## Quantiles: \n\n", paste(out_quant, collapse = "\n"), "\n\n",
    "## Value and Type Columns: \n\n", paste(out_val, collapse = "\n"), "\n\n",
    "## Target Columns: \n\n", paste(out_target, collapse = "\n"), "\n\n",
    "## Locations: \n\n", paste(out_loc, collapse = "\n"), "\n\n"
  )
  #Output:
  if (!(all(grepl("^No error", c(out_col, out_scen, out_mpd, out_quant,
                                 out_val, out_target, out_loc))))) {
    if (any(grepl("\U000274c Error", c(out_col, out_scen, out_mpd, out_quant,
                                       out_val, out_target, out_loc)))) {
      cat(test_report)
      stop(" The submission contains one or multiple issues, please see ",
           "information above")
    } else {
      cat(test_report)
      warning(" The submission is accepted but contains some warnings, please ",
              "verify the information above")
    }
  } else {
    print("End of validation check: all the validation checks were successfull")
  }
}



#' Validate SMH (Scenario Modeling Hub) Submissions
#'
#'
#' Runs all the different validation checks functions (test_column,
#' test_scenario, test_modelprojdate, test_quantiles, test_val, test_target,
#' test_location) on a Scenario Modeling Hub (SMH) submissions and prints
#' information about the results of each tests on the submission: warning(s),
#' error(s) or if all the tests were successful.
#'
#'@param path path to the submissions file to test
#'@param lst_gs named list of data frame containing the
#' observed data. We highly recommend to use the output of the pull_gs_data()
#' function. The list should have the same format: each data frame should be
#' named with the corresponding covidcast signal except "hospitalization"
#' instead of "confirmed_admissions_covid_1d".
#'@param pop_path path to a table containing the population size of each
#'  geographical entities by FIPS (in a column "location") and by location name.
#'  By default, path to the locations file in the COVID19 Scenario Modeling Hub
#'  GitHub repository
#'@param scen_info NULL, character vector (path leading to a csv file) or data
#'  frame, containing, the round and scenario information in the same output
#'  format as the function `scen_round_info()`. Please see documentation for
#'  more information. The default is NULL. If NULL, the information will be
#'  directly and automatically extracted from the Scenario Modeling Hub GitHub
#'  repository.
#'
#'@details For more information on all tests run on the submission, please refer
#' to the documentation of each "test_*" function. A vignette with all the
#' information might be created later on too.
#' \cr\cr
#' If the `scen_info` parameter is set to NULL, the information is extracted
#' from the multiple README from the Scenario Modeling Hub GitHub repository by
#' using the GitHub API. Just as a warning, the number of call is limited to
#' 60 per hour. If you plan to use the `validate_submission()` function multiple
#' times in a short time frame, we advise use to store the scenario information
#' in a data frame by doing `df_scen_info <-  scen_round_info()` for example,
#' and setting the `scen_info` parameter to `scen_info = df_scen_info`.
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom stats setNames
#'
#'@examples
#' \dontrun{
#' lst_gs <- pull_gs_data()
#' validate_submission("PATH/TO/SUBMISSION", lst_gs)
#' }
#'@export
validate_submission <- function(path,
                                lst_gs,
                                pop_path = "https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/master/data-locations/locations.csv",
                                scen_info = NULL) {

  # Prerequisite --------
  # Load gold stantard data
  lst_gs <- lapply(seq_along(lst_gs), function(x) {
    df_gs <- dplyr::mutate(lst_gs[[x]],
                           target_name = names(lst_gs[x])) %>%
      dplyr::select(time_value, value, location = fips, target_name)
  }) %>%
    setNames(names(lst_gs))
  # Pull population data and prepare location hash vector
  pop <- read_files(pop_path)
  number2location <- setNames(pop$location_name, pop$location)
  # Pull Scenario data and prepare scenario hash vector
  if (is.null(scen_info)) {
    scen_df <- scen_round_info()
  } else {
    if (is.vector(scen_info)) {
      scen_df <- read_files(scen_info)
    } else if (is.data.frame(scen_info)) {
      scen_df <- scen_info
    } else {
      stop("`scen_info` paramater should either be: a path to a file containing ",
           "the round and scenario information, or a data frame, or NULL. If ",
           "NULL, the information will be extracted from the Scenario Modeling",
           " Hub GitHub repository, ")
    }
  }


  scenario_smname <- setNames(scen_df$scenario_name, scen_df$scenario_id)
  scenario_sel <- setNames(scen_df$scenario_id, scen_df$round)

  # Print message --------
  print(paste0("Run validation on file: ", basename(path)))

  # Process file to test and associated information --------
  # Read file
  df <- suppressMessages(read_files(path))
  # Extract round information
  round <- as.numeric(
    gsub("[^[:digit:]]", "", unique(scen_df[which(
      scen_df$first_week_ahead == as.Date(basename(path)) + 6), "round",
      TRUE])))
  if (length(round) == 0) {
    round <- as.numeric(gsub("[^[:digit:]]", "", unique(scen_df[which(
      scen_df$scenario_id %in% unique(df$scenario_id)), "round"])))
  }
  if (length(round) == 0) {
    stop("Cannot extract round number information from the files, please ",
         "verify file name and scenario id information. If the problem ",
         "persists please leave a message tagging '@LucieContamin'.")
  }
  # Extract start_date information
  start_date <- as.Date(unique(scen_df[which(
    scen_df$round == paste0("round", round)), "first_week_ahead", TRUE]))
  # Extract w0 or w-1 of observed data
  last_week_gs <-  lapply(lst_gs, function(x) {
    lastw_df <- dplyr::filter(x, time_value < start_date) %>%
      dplyr::filter(time_value == max(time_value)) %>%
      dplyr::select(last_value = value, location, target_name)
  })

  # Run tests --------
  run_all_validation(df, round, start_date, path = path, pop = pop,
                     last_lst_gs = last_week_gs,
                     scenario_smname = scenario_smname,
                     scenario_sel = scenario_sel,
                     number2location =number2location)
}

