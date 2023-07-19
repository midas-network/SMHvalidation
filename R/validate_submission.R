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
#'@details Internal function called in the `validation_submission()` function.
#' For more information on all tests run on the submission, please refer to the
#' documentation of each "test_*" function. A vignette with all the information
#' might be created later on too.
#'
#' @noRd
run_all_validation <- function(df, path, pop, last_lst_gs,
                               number2location, js_def) {
  ### Prerequisite
  model_task <- js_def$model_tasks
  task_ids <- purrr::map(model_task, "task_ids")
  req_colnames <-  c(unique(names(unlist(task_ids, FALSE))), "output_type", "output_type_id",
                     "value")

  ### Tests:
  # Test on column information (name and number)
  out_col <- test_column(df, req_colnames)
  # update to stop if any missing column
  if (length(colnames(df)) < length(req_colnames)) {
    cat(out_col)
    stop(" The submission contains an issue, the validation was not run, please",
         " see information above.")
  }
  # select only required column for the other tests
  df <- df[, req_colnames]

  #if (isFALSE(dim(df[!duplicated(df),])[1] == dim(df)[1])) {
  #  warning("The submission file contains some duplicated row.")
  #}

  # Test on Scenario information
  out_scen <- test_scenario(df, task_ids)

  # Test origin date information
  out_ord <- test_origindate(df, path, id = js_def$round_id)

  # Test by type
  if (any(grepl("quantile", unlist(distinct(df[ ,"output_type", FALSE])))) |
      any("quantile" %in% names(unlist(purrr::map(model_task, "output_type"),
                                       FALSE)))) {
    out_quant <- test_quantiles(df, model_task)
  }
  if (any(grepl("sample", unlist(distinct(df[ ,"output_type", FALSE])))) |
      any("sample" %in% names(unlist(purrr::map(model_task, "output_type"),
                                     FALSE)))) {
    out_sample <- test_sample(df, model_task)
  }

  # Test on value
  out_val <- test_val(df, pop, last_lst_gs, model_task)

  # Test on targets information
  out_target <- test_target(df, model_task)

  # Test on location information
  out_loc <- test_location(df, number2location, task_ids)

  # Test for additional column
  if (any(grepl("age_group", names(df)))) {
    out_agegroup <- test_agegroup(df, model_task)
  }

  # Report:
  test_report <- paste(
    "\n ## Columns: \n\n", paste(out_col, collapse = "\n"),
    "\n\n## Scenarios: \n\n", paste(out_scen, collapse = "\n"),
    "\n\n## Origin Date Column:  \n\n", paste(out_ord, collapse = "\n"),
    "\n\n## Value and Type Columns: \n\n", paste(out_val, collapse = "\n"),
    "\n\n## Target Columns: \n\n", paste(out_target, collapse = "\n"),
    "\n\n## Locations: \n\n", paste(out_loc, collapse = "\n"))
  if (any(grepl("sample", unlist(distinct(df[ ,"output_type", FALSE])))) |
      any("sample" %in% names(unlist(purrr::map(model_task, "output_type"),
                                     FALSE))))  {
    test_report <- paste(
      test_report, "\n\n## Sample: \n\n", paste(out_sample, collapse = "\n"))
  }
  if (any(grepl("quantile", unlist(distinct(df[ ,"output_type", FALSE])))) |
      any("quantile" %in% names(unlist(purrr::map(model_task, "output_type"),
                                     FALSE))))  {
    test_report <- paste(
      test_report, "\n\n## Quantiles: \n\n", paste(out_quant, collapse = "\n"))
  }
  if (any(grepl("age_group", names(df)))) {
    test_report <- paste(
      test_report,
      "\n\n## Age Group: \n\n", paste(out_agegroup, collapse = "\n"))
  }
  test_report <- paste0(test_report, "\n\n")

  # Output:
  if (any(grepl("\\\U000274c Error|\\\U0001f7e1 Warning", test_report))) {
    if (any(grepl("\U000274c Error", test_report))) {
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
#'@param path path to the submissions file to test or string of parquet files (
#' in this case, the validation will be run on the aggregation of all the
#' parquet files together)
#'@param js_def path to JSON file containing round definitions: names of
#'  columns, target names, ...
#'@param lst_gs named list of data frame containing the
#' observed data. For COVID-19, we highly recommend to use the output of the
#' pull_gs_data() function. The list should have the same format: each data
#' frame should be named with the corresponding covidcast signal except
#' "hospitalization" instead of "confirmed_admissions_covid_1d".
#'@param pop_path path to a table containing the population size of each
#'  geographical entities by FIPS (in a column "location") and by location name.
#'
#'@details For more information on all tests run on the submission, please refer
#' to the documentation of each "test_*" function. A vignette with all the
#' information might be created later.
#'
#' The function accepts submission in PARQUET, CSV, ZIP or GZ file formats.
#'
#' The function runs some preliminary tests before calling the "test_*"
#' functions:
#' \itemize{
#'  \item{Input submission file format: }{The file format of the submission
#'  file(s) correspond to the expected format (for example: `parquet`, or `csv`,
#'   etc.). If multiple files inputted, only `parquet` is accepted}
#'   \item{Date information: }{The column `origin_date` in the submission file
#'   corresponds to a `model_tasks` round information in the JSON file
#'   (`js_def` parameter)}
#'   \item{Date format: }{All columns containing dates information should be in
#'   "YYY-MM-DD" format}
#' }
#'
#' @importFrom dplyr mutate select %>% mutate_all distinct collect
#' @importFrom stats setNames
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @importFrom purrr list_simplify
#'
#'@examples
#' \dontrun{
#' # FOR SMH Submission
#'
#' lst_gs <- pull_gs_data()
#' pop_path <- "PATH/TO/data-locations/locations.csv"
#' js_def <- "PATH/TO/tasks.json"
#'
#' validate_submission("PATH/SUBMISSION", js_def, lst_gs, pop_path)
#'
#' }
#'@export
validate_submission <- function(path, js_def, lst_gs, pop_path) {

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
  js_def <- jsonlite::fromJSON(js_def, simplifyDataFrame = FALSE)

  # Print message --------
  print(paste0("Run validation on file: ",
               paste(basename(path), collapse = ", ")))

  # Process file to test and associated information --------
  # Read file
  if (length(path) == 1) {
    df <- read_files(path) %>%
      dplyr::mutate_if(is.factor, as.character)
  } else if (all(grepl("parquet", path))) {
    ds <- arrow::open_dataset(path, format = "parquet")
    df <- dplyr::collect(ds)  %>%
      dplyr::mutate_if(is.factor, as.character)
  } else {
    err003 <- paste0(
      "\U000274c Error 005: The file format of the submission was not ",
      "recognized, please use one unique files or multiple parquet file",
      ". For more information, please look at the documentation of the ",
      "hub. \n")
    cat(err003)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }

  # test date format
  df <- as.data.frame(df)
  if (any(is.na(as.Date(na.omit(unlist(dplyr::mutate_all(
    dplyr::distinct(df[, grepl("date", names(df)), FALSE]), as.character))),
    "%Y-%m-%d")))) {
    err003 <- paste0(
      "\U000274c Error 003: The columns containing date information should be
      in a date format `YYYY-MM-DD`. Please verify \n")
    cat(err003)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }

  # Select the associated round (add error message if no match)
   team_round <- as.Date(df$origin_date[[1]])
   js_date <- lapply(js_def$rounds, function(x) {
     if (x$round_id_from_variable) {
       js_date <- as.Date(x$model_tasks[[1]]$task_ids[[x$round_id]]$required)
     } else {
       js_date <- as.Date(x$round_id)
     }
   })
   js_date <- purrr::list_simplify(js_date)
   if (team_round %in% js_date) {
     js_def <- js_def$rounds[js_date %in% team_round]
     js_def <- js_def[[1]]
   } else {
     err004 <- paste0(
       "\U000274c Error 004: The origin_date in the submission file was not ",
       "associated with any task_ids round. Please verify the date information",
       " in the origin_date column corresponds to the expected value.\n")
     cat(err004)
     stop(" The submission contains an issue, the validation was not run, ",
          "please see information above.")
   }

  # Extract week 0 or week -1 of observed data
  last_week_gs <-  lapply(lst_gs, function(x) {
    lastw_df <- dplyr::filter(x, time_value < as.Date(js_def$round_id) + 6) %>%
      dplyr::filter(time_value == max(time_value)) %>%
      dplyr::select(last_value = value, location, target_name)
  })

  # Run tests --------
   run_all_validation(df, path = path, pop = pop,
                     last_lst_gs = last_week_gs,
                     number2location = number2location, js_def = js_def)
}

