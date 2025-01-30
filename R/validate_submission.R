#' Merge and simple tests of Sample ID columns
#'
#' If the submission files contains the multiple columns for sample ID, the
#' function will be called to: combines the columns into one `output_type_id`
#' column (on `"sample"` output type only) and tests if: the columns names are
#' correct, the two columns contains only integer values,and if the
#' concatenation does not returns an unique value.
#'
#' The function returns a named list with: the output data frame (without the
#' two columns, `"df"`) and a list of error message, if any issue
#' (`"add_message"`)
#'
#' @param df data frame
#' @param req_colnames character vector of the expected column names
#' @param merge_sample_col character vector of the sample ID column names to
#'  concatenate
#' @param add_meesage character vector, error message to append
#'
#' @importFrom tidyr unite matches
#' @importFrom dplyr mutate select .data
#'
#' @noRd
merge_sample_id <- function(df, req_colnames, merge_sample_col,
                            add_message = NULL) {
  # Validation
  if (!(all(c(req_colnames, merge_sample_col) %in% names(df)))) {
    fail_col <- req_colnames[!req_colnames %in% names(df)]
    colnames_test <-
      paste0("\U000274c Error 101: At least one column name is misspelled or",
             " does not correspond to the expected column names. The ",
             "column(s) ", paste(fail_col, collapse = ", "),
             " do(es) not correspond to the standard")
    cat(colnames_test)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }
  sample_val <- na.omit(unlist(distinct(df[, merge_sample_col])))
  if (isFALSE(all(is_wholenumber(sample_val))) ||
        any(startsWith(as.character(sample_val), "0"))) {
    err_message <-
      paste0("\U000274c Error 903: The columns ",
             paste(merge_sample_col, collapse = " and "),
             " should contain integer values only for type ",
             "'sample'. Please verify")
    add_message <- paste(add_message, err_message, sep = "\n")
  }
  # Merge sample ID column
  df <- tidyr::unite(df, col = "type_id_sample", merge_sample_col) |>
    dplyr::mutate(output_type_id =
                    ifelse(.data[["output_type"]] == "sample",
                           as.numeric(as.factor(.data[["type_id_sample"]])),
                           .data[["output_type_id"]])) |>
    dplyr::select(-tidyr::matches("type_id_sample"))
  if (length(unique(df[which(df$output_type == "sample"),
                       "output_type_id"])) <= 1) {
    add_message <- paste0(add_message,
                          "\n\U000274c Error 902: The submission should ",
                          "contains multiple sample output type groups, ",
                          "please verify.\n")
  }
  return(list("df" = df, "add_message" = add_message))
}

#' Create the validation report
#'
#' Combine all the test output function into one report
#'
#' @param df data frame to test
#' @param js_def list containing round definitions: names of columns,
#' target names, ...
#' @param out_req character vector, error message about the required values
#'  to append to the report
#' @param out_col character vector, error message about the columns
#'  to append to the report
#' @param out_scen character vector, error message about the `scenario_id`
#'  column to append to the report
#' @param out_ord character vector, error message about the `origin_date` column
#'  to append to the report
#' @param out_val character vector, error message about the `value` column
#'  to append to the report
#' @param out_target character vector, error message about the `target` column
#'  to append to the report
#' @param out_loc character vector, error message about the `location` column
#'  to append to the report
#' @param out_type character vector, error message about all output
#' type to append to the report
#' @param out_other character vector, error message about additional
#' column to append to the report. Uses only if the submission is expected to
#' contains a additional columns (than previously tested)
#' @param add_message character vector, error message to append to the report,
#' by default NULL, no additional error message to append
#'
#' @noRd
create_report <- function(df, js_def, out_req, out_col,
                          out_scen, out_ord, out_val, out_target, out_loc,
                          out_type, out_other, add_message = NULL) {

  test_report <-
    paste("\n ## Required values: \n", paste(out_req, collapse = "\n"),
          "\n\n ## Columns: \n", paste(out_col, collapse = "\n"),
          "\n\n## Scenarios: \n", paste(out_scen, collapse = "\n"),
          "\n\n## Origin Date Column:  \n", paste(out_ord, collapse = "\n"),
          "\n\n## Value and Type Columns: \n", paste(out_val, collapse = "\n"),
          "\n\n## Target Columns: \n", paste(out_target, collapse = "\n"),
          "\n\n## Locations: \n", paste(out_loc, collapse = "\n"),
          "\n\n## Output Type: \n", paste(out_type, collapse = "\n"))
  if (!is.null(out_other) || !is.null(add_message)) {
    test_report <- paste(test_report, "\n\n## Other Information: \n",
                         paste(out_other, add_message, collapse = "\n"))
  }
  test_report <- paste0(test_report, "\n\n")
  return(test_report)
}

#' Run all validation checks and output a report
#'
#' Runs all the different validation checks functions (test_column,
#' test_scenario, test_modelprojdate, test_quantiles, test_val, test_target,
#' test_location, etc.) on a data frame and print information about the results
#' of each tests on the submission: warning(s), error(s) or if all the tests
#' were successful.
#'
#'@param df data frame to test
#'@param path character vector path of the file being tested
#'@param js_def list containing round definitions: names of columns,
#' target names, ...
#'@param pop data frame containing the population size of each geographical
#'  entities by fips (in a column "location"), can be `NULL` (not comparison to
#'  population size, default)
#'@param obs data frame, containing the last available week of observed data
#'  before start of the projection, can be `NULL` (not comparison to observed
#'  data, default) in the time series hubverse format.
#'@param merge_sample_col vector to indicate if the for the output type
#' "sample", the output_type_id column is set to NA and the information is
#' contained into other columns. If so the parameter should be set to the sample
#' ID columns names, for example: `c("run_grouping" and "stochastic_run")`.
#' By default, `NULL` (sample ID information in the output type id column)
#'@param pairing_col column names indicating the sample pairing information. By
#' default: "horizon".
#'@param n_decimal integer,  number of decimal point accepted in the column
#'  value (only for "sample" output type), if NULL (default) no limit expected.
#'@param verbose Boolean, if TRUE add information about the sample pairing
#'  information in output message
#'
#'@details Internal function called in the `validation_submission()` function.
#' For more information on all tests run on the submission, please refer to the
#' documentation of each "test_*" function. A vignette with all the information
#' might be created later on too.
#'
#' @noRd
run_all_validation <- function(df, path, js_def, pop = NULL, obs = NULL,
                               merge_sample_col = NULL, pairing_col = "horizon",
                               n_decimal = NULL, verbose = TRUE) {
  ### Prerequisite
  req_colnames <-  c(get_tasksids_colnames(js_def), "output_type",
                     "output_type_id", "value")

  # Merge sample ID column
  if (!is.null(merge_sample_col)) {
    sample_update <- merge_sample_id(df, req_colnames, merge_sample_col,
                                     add_message = NULL)
    df <- sample_update[["df"]]
    add_message <- sample_update[["add_message"]]
  } else {
    df <- df
    add_message <- NULL
  }

  ### Tests:
  # Test on column information (name and number)
  out_col <- test_column(df, req_colnames)
  # select only required column for the other tests
  df <- df[, req_colnames]

  # Test missing required value
  out_req <- test_req_value(df, js_def)

  # Test on Scenario information
  out_scen <- test_scenario(df, js_def)

  # Test origin date information
  out_ord <-
    test_origindate(df, path,
                    id = unique(unlist(purrr::map(purrr::map(js_def,
                                                             "task_ids"),
                                                  "origin_date"))))

  # Test by output type
  out_type <- test_type(df, js_def, pairing_col = pairing_col,
                        verbose = verbose)

  # Test on value
  out_val <- test_val(df, pop, obs, js_def, n_decimal = n_decimal)

  # Test on targets information
  out_target <- test_target(df, js_def)

  # Test on location information
  out_loc <- test_location(df, js_def)

  # Test for additional column
  out_other <- test_other_col(df, js_def)

  # Report:
  test_report <- create_report(df, js_def, out_req, out_col, out_scen, out_ord,
                               out_val, out_target, out_loc, out_type,
                               out_other, add_message = add_message)

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
    test_report <-
      "End of validation check: all the validation checks were successful\n"
    cat(test_report)
  }
}


#' Validate SMH (Scenario Modeling Hub) Submissions
#'
#' Runs all the different validation checks functions (`test_column()`,
#' `test_scenario()`, `test_origindate()`, `test_type()`, `test_val()`,
#' `test_target()`, `test_location()`, `test_other_col()`) on SMH submission
#' file(s) and prints information about the results of each tests on the
#' submission file(s): warning(s), error(s) or message if all the tests were
#' successful.
#'
#'@param path path to the submissions file (or folder for partitioned data)
#' to test, or string of parquet files (in this case, the validation will be
#' run on the aggregation of all the parquet files together, and each file
#' individually should match the expected SMH standard).
#' If partition is not set to `NULL`, path to the folder containing ONLY the
#' partitioned data.
#'@param js_def path to JSON file containing round definitions: names of
#' columns, target names, ... following the `tasks.json`
#' [Hubverse
#' ](https://hubdocs.readthedocs.io/en/latest/user-guide/hub-config.html)
#' format
#'@param lst_gs named list of data frame containing the
#' observed data. For COVID-19, we highly recommend to use the output of the
#' `pull_gs_data()` function. The list should have the same format: each data
#' frame should be named with the corresponding covidcast signal except
#' `"hospitalization"` instead of `"confirmed_admissions_covid_1d"`.
#' Set to `NULL` (default), to NOT run comparison with observed data.
#'@param pop_path path to a table containing the population size of each
#' geographical entities by FIPS (in a column `"location"`).
#' Use to compare that value is not higher than expected population size.
#' Set to `NULL` (default), to NOT run comparison with population data.
#'@param merge_sample_col vector to indicate if the for the output type
#' "sample", the output_type_id column is set to NA and the information is
#' contained into other columns. If so the parameter should be set to the sample
#' ID columns names, for example: `c("run_grouping" and "stochastic_run")`.
#' By default, `NULL` (sample ID information in the output type id column)
#'@param partition vector, for csv and parquet files, allow to validate files
#' in a partition format, see `arrow` package for more information, and
#' `arrow::write_dataset()`, `arrow::open_dataset()` functions.  By default
#' `NULL`, no partition
#'@param n_decimal integer,  number of decimal point accepted in the column
#' value (only for `"sample"` output type), if `NULL` (default) no limit
#' expected.
#'@param round_id character string, round identifier. If `NULL` (default),
#' extracted from `path`.
#'@param verbose Boolean, if TRUE add information about the sample pairing
#'  information in output message. By default, `TRUE` (slows the validation
#'  validation for sample output type)
#'@param r_schema Schema arrow objects, to read partition files with a specific
#' schema. If none provided (default, `NULL`), the schema will be created from
#' the `js_def` JSON file. (only for partition files)
#'
#'@details For more information on all tests run on the submission, please refer
#' to the documentation of each `"test_*`" function. A vignette with all the
#' information is available in the package and is called:
#' vignette("validation-checks").
#'
#' The function accepts submission in PARQUET, CSV, ZIP or GZ file formats. If
#' the submission files is in a "partitioned" format, the `path` parameter
#' should be to a directory to a folder containing ONLY the "partitioned"
#' files. If any other file is present in the directory, it will be included
#' in the validation.
#'
#' The function runs some preliminary tests before calling the "test_*"
#' functions:
#' * Input submission file format: The file format of the submission
#'  file(s) correspond to the expected format (for example: `parquet`, or `csv`,
#'   etc.). If multiple files inputted, only `parquet` is accepted.
#'  * Date information: The column `origin_date` in the submission file
#'   corresponds to a `model_tasks` round information in the JSON file
#'   (`js_def` parameter).
#'  * Date format: All columns containing dates information should be in
#'   "YYY-MM-DD" format.
#'
#' @importFrom hubUtils get_round_ids read_config_file get_round_model_tasks
#' @importFrom stringr str_extract
#' @importFrom arrow open_dataset
#' @importFrom dplyr collect mutate_if mutate_all distinct
#'
#'@examples
#' \dontrun{
#' validate_submission("PATH/TO/SUBMISSION", "PATH/TO/ROUND/tasks.json")
#' }
#'@export
validate_submission <- function(path, js_def, target_data = NULL,
                                pop_path = NULL, merge_sample_col = NULL,
                                partition = NULL, n_decimal = NULL,
                                round_id = NULL, verbose = TRUE,
                                r_schema = NULL) {

  # Prerequisite --------
  # Pull target data
  if (!is.null(target_data)) obs <- read_files(target_data) else obs <- NULL
  # Pull population data
  if (!is.null(pop_path)) pop <- read_files(pop_path) else pop <- NULL

  # Read hub config JSON file ------
  js_def0 <- hubUtils::read_config_file(js_def)
  # Select the associated round (add error message if no match)
  round_id <- team_round_id(path, round_id = round_id, partition = partition)
  if (round_id %in% hubUtils::get_round_ids(js_def0)) {
    js_def <- hubUtils::get_round_model_tasks(js_def0, as.character(round_id))
  } else {
    err004 <-
      paste0("\U000274c Error 004: The round id (date in the submission file) ",
             "is not associated with any task_ids round. Please verify the ",
             "date information in 'path' or in the 'round_id' parameter.\n",
             "Possible round ids are: ",
             paste(unique(hubUtils::get_round_ids(js_def0)), collapse = ", "))
    cat(err004)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }

  # Select validation file(s) and print message --------
  if (!is.null(partition)) {
    file_path <- grep(round_id, dir(path, recursive = TRUE), value = TRUE) |>
      unique()
    file_path <- c(file_path[1:min(max(length(file_path) - 1, 1), 5)], "etc.")
    cat(paste0("Run validation on files: ",
               paste(file_path, collapse = ", "), "\n"))
  } else {
    file_path <- basename(path)
    cat(paste0("Run validation on files: ", paste(unique(file_path),
                                                  collapse = ", "), "\n"))
  }

  # Process file to test and associated information --------
  # Read file
  if (length(path) == 1 &&
        all(grepl(".csv$|.zip$|.gz$|.pqt$|.parquet$", path))) {
    df <- read_files(path)
  } else if (all(grepl("parquet$", path))) {
    ds <- arrow::open_dataset(path, format = "parquet")
    df <- dplyr::collect(ds) |>
      dplyr::mutate_if(is.factor, as.character)
  } else if (!is.null(partition)) {
    df <- load_partition_arrow(path, js_def = js_def0, js_def_round = js_def,
                               partition = partition, round_id = round_id,
                               merge_sample_col = merge_sample_col,
                               r_schema = r_schema)
  } else {
    err005 <-
      paste0("\U000274c Error 005: The file format of the submission was not ",
             "recognized, please use one unique files or multiple parquet file",
             ". For more information, please look at the documentation of the ",
             "hub. \n")
    cat(err005)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }
  # File format
  # Test if any factor columns
  if (any(sapply(colnames(df), function(x) is.factor(df[[x]])))) {
    col_message <- paste0("\n\U000274c Error 104: At least one column is in a ",
                          "format: 'factor', please verify")
    cat(col_message)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  } else {
    col_message <- NULL
  }
  df <- dplyr::mutate_if(df, is.factor, as.character)


  # test date format
  test_date <- df[, grepl("date", names(df)), FALSE]
  test_date <- unlist(dplyr::mutate_all(dplyr::distinct(test_date),
                                        as.character))
  if (any(is.na(as.Date(na.omit(test_date), "%Y-%m-%d")))) {
    err003 <-
      paste0("\U000274c Error 003: The columns containing date information ",
             "should be in a date format `YYYY-MM-DD`. Please verify \n")
    cat(err003)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }

  # Extract pairing information
  sample_js <- purrr::map(purrr::map(js_def, "output_type"), "sample")
  tasks_ids <- get_tasksids_colnames(js_def)
  if (!all(is.null(sample_js))) {
    sample_list <- purrr::map(sample_js, "output_type_id_params")
    comp_col <- purrr::map(sample_list, "compound_taskid_set")
    if (!all(purrr::map_vec(comp_col, is.null))) {
      pairing_col <- tasks_ids[!tasks_ids %in% unique(unlist(comp_col))]
    } else {
      pairing_col <- "horizon"
    }
  }
  # Extract week 0 or week -1 of observed data
  or_date <- unique(unlist(purrr::map_depth(js_def, 2, "origin_date")))
  if (!is.null(obs)) {
    obs <- dplyr::filter(obs, date < as.Date(or_date) + 6) |>
      dplyr::filter(date == max(date)) |>
      dplyr::select(dplyr::all_of(c("observation", "location", "signal")))
  }

  # Run tests --------
  run_all_validation(df, path = path, js_def = js_def, pop = pop,
                     last_lst_gs = obs, merge_sample_col = merge_sample_col,
                     pairing_col = pairing_col, n_decimal = n_decimal,
                     verbose = verbose)
}
