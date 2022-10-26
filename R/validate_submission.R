#' Run all validation checks and output a report
#'
#' Runs all the different validation checks functions (test_column,
#' test_scenario, test_modelprojdate, test_quantiles, test_val, test_target,
#' test_location) on a data frame and print information about the results of
#' each tests on the submission: warning(s), error(s) or if all the tests
#' were successful.
#'
#'@param df data frame to test
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
#'@param js_def list containing round definitions: names of columns,
#' target names, ...
#'
#'@details Internal function called in the `validation_submission()` function. For
#' more information on all tests run on the submission, please refer to the
#' documentation of each "test_*" function. A vignette with all the information
#' might be created later on too.
#'
#' @noRd
run_all_validation <- function(df, start_date, path, pop, last_lst_gs,
                               number2location, js_def) {
  # Tests:
  out_col <- test_column(df, js_def)
  # add missing age_group column for all the tests
  if (any(!js_def$column_names %in% names(df))) {
    if (js_def$column_names[!js_def$column_names %in%
                            names(df)] == "age_group") {
      df[, "age_group"] <- "0-130"
    }
  }
  # select only required column for the other tests
  df <- df[, js_def$column_names]

  out_scen <- test_scenario(df,js_def)
  out_mpd <- test_modelprojdate(df, path, start_date)
  if (any(grepl("quantile", js_def$column_names))) {
    out_quant <- test_quantiles(df, js_def)
  } else {
    out_quant <- paste0("No 'quantile' information required, no validation ",
                        "runs for Quantiles information")
  }
  if (any(grepl("sample", js_def$column_names))) {
    out_sample <- test_sample(df, js_def)
  } else {
    out_sample <- paste0("No 'sample' information required, no validation runs",
                         " for Sample information")
  }
  out_val <- test_val(df, pop, last_lst_gs, js_def)
  out_target <- test_target(df, start_date, js_def)
  out_loc <- test_location(df, number2location, js_def)
  if (any(grepl("age_group", js_def$column_names))) {
    out_agegroup <- test_agegroup(df, js_def)
  } else {
    out_agegroup <- paste0("No 'age_group' information required, no validation",
                           " runs for age group information")
  }
  # Report:
  test_report <- paste(
    "\n ## Columns: \n\n", paste(out_col, collapse = "\n"),
    "\n\n## Scenarios: \n\n", paste(out_scen, collapse = "\n"), "\n\n",
    "## Model Projection Date Column:  \n\n", paste(out_mpd, collapse = "\n"),
    "\n\n## Quantiles: \n\n", paste(out_quant, collapse = "\n"),
    "\n\n## Sample: \n\n", paste(out_sample, collapse = "\n"),
    "\n\n## Value and Type Columns: \n\n", paste(out_val, collapse = "\n"),
    "\n\n## Target Columns: \n\n", paste(out_target, collapse = "\n"),
    "\n\n## Locations: \n\n", paste(out_loc, collapse = "\n"),
    "\n\n## Age Group: \n\n", paste(out_agegroup, collapse = "\n"),
    "\n\n")
  #Output:
  if (!(all(grepl("^No error|^No .+ required",
                  c(out_col, out_scen, out_mpd, out_quant, out_val, out_target,
                    out_loc, out_sample, out_agegroup))))) {
    if (any(grepl("\U000274c Error", c(out_col, out_scen, out_mpd, out_quant,
                                       out_val, out_target, out_loc, out_sample,
                                       out_agegroup)))) {
      cat(test_report)
      stop(" The submission contains one or multiple issues, please see ",
           "information above")
    } else {
      cat(test_report)
      warning(" The submission is accepted but contains some warnings, please ",
              "verify the information above")
    }
  } else {
    print("End of validation check: all the validation checks were successful")
  }
}


#' Validate SMH (Scenario Modeling Hub) Submissions
#'
#' Runs all the different validation checks functions (test_column,
#' test_scenario, test_modelprojdate, test_quantiles, test_val, test_target,
#' test_location, test_sample, test_agegroup) on a Scenario Modeling Hub (SMH)
#' submissions and prints information about the results of each tests on the
#' submission: warning(s), error(s) or message if all the tests were successful.
#'
#'@param path path to the submissions file to test
#'@param lst_gs named list of data frame containing the
#' observed data. For COVID-19, we highly recommend to use the output of the
#' pull_gs_data() function. The list should have the same format: each data
#' frame should be named with the corresponding covidcast signal except
#' "hospitalization" instead of "confirmed_admissions_covid_1d".
#'@param pop_path path to a table containing the population size of each
#'  geographical entities by FIPS (in a column "location") and by location name.
#'@param js_def path to JSON file containing round definitions: names of
#'  columns, target names, ...
#'
#'@details For more information on all tests run on the submission, please refer
#' to the documentation of each "test_*" function. A vignette with all the
#' information might be created later on too.
#' \cr\cr
#' The function accepts submission in CSV, ZIP or GZ file formats.
#'
#' @importFrom dplyr mutate select %>% mutate_all
#' @importFrom stats setNames
#' @importFrom jsonlite fromJSON
#'
#'@examples
#' \dontrun{
#' # FOR COVID-19 SMH Submission
#'
#' lst_gs <- pull_gs_data()
#' pop_path <- "PATH/TO/data-locations/locations.csv"
#' js_def <- "PATH/TO/covid.json"
#'
#' validate_submission("PATH/SUBMISSION", lst_gs, pop_path, js_def)
#'
#' # FOR FLU SMH Submission
#'
#' lst_gs <- NULL
#' pop_path <- "PATH/TO/data-locations/locations.csv"
#' js_def <- "PATH/TO/flu.json"
#'
#' validate_submission("PATH/SUBMISSION", lst_gs, pop_path, js_def)
#'
#' }
#'@export
validate_submission <- function(path,
                                lst_gs,
                                pop_path,
                                js_def) {

  # Prerequisite --------
  # Load gold standard data
  lst_gs <- lapply(seq_along(lst_gs), function(x) {
    df_gs <- dplyr::mutate(lst_gs[[x]],
                           target_name = names(lst_gs[x])) %>%
      dplyr::select(time_value, value, location = fips, target_name)
  }) %>%
    setNames(names(lst_gs))

  # Pull population data and prepare location hash vector
  pop <- read_files(pop_path)
  number2location <- setNames(pop$location_name, pop$location)

  # Read JSON file
  js_def <- jsonlite::fromJSON(js_def)

  # Print message --------
  print(paste0("Run validation on file: ", basename(path)))

  # Process file to test and associated information --------
  # Read file
  df <- read_files(path) %>%
    dplyr::mutate_if(is.factor, as.character)

  # test date format
  if (any(is.na(as.Date(na.omit(unlist(dplyr::mutate_all(
    df[, grepl("date", names(df))], as.character))), "%Y-%m-%d")))) {
    err003 <- paste0(
      "\U000274c Error 003: The columns containing date information should be
      in a  should be in a date format `YYYY-MM-DD`. Please verify")
    cat(err003)
    stop(" The submission contains am issue, the validation was not run, please",
         " see information above.")
  }

  # Prepare information per round
  start_date <- as.Date(js_def$first_week_ahead)

  # Extract week 0 or week -1 of observed data
  last_week_gs <-  lapply(lst_gs, function(x) {
    lastw_df <- dplyr::filter(x, time_value < start_date) %>%
      dplyr::filter(time_value == max(time_value)) %>%
      dplyr::select(last_value = value, location, target_name)
  })

  # Run tests --------
  run_all_validation(df, start_date = start_date, path = path, pop = pop,
                     last_lst_gs = last_week_gs,
                     number2location = number2location, js_def = js_def)
}

