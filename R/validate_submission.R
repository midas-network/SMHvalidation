#' Merge and simple tests of Sample ID columns
#'
#' If the submission files contains the two `"run_grouping"` and
#'  `"stochastic_run"` columns for sample ID, the function will be called to:
#'  combines the two columns into one `output_type_id` column and tests if:
#'  the columns names are correct, the two columns contains only integer values
#'  and not an unique value.
#'
#' The function returns a named list with: the output data frame (without the
#' two columns, `"df"`) and a list of error message, if any issue
#' (`"add_message"`)
#'
#' @param df data frame to test
#' @param req_colnames character vector of the expected column names
#' @param add_meesage character vector, error message to append
#'
#' @noRd
merge_sample_id <- function(df, req_colnames, add_message = NULL) {
  if (!(all(c(req_colnames, "run_grouping", "stochastic_run") %in%
              names(df)))) {
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
  if (isFALSE(all(is_wholenumber(na.omit(df$run_grouping)))) ||
        isFALSE(all(is_wholenumber(na.omit(df$stochastic_run))))) {
    err_message <-
      paste0("\U000274c Error 903: The column 'run_grouping' and ",
             "'stochastic_run' should contain integer values only for type ",
             "'sample'. Please verify")
    add_message <- paste(add_message, err_message, sep = "\n")
  }
  df <-
    dplyr::mutate(df,
                  output_type_id = ifelse(output_type == "sample",
                                          as.factor(paste0(run_grouping, "-",
                                                           stochastic_run)),
                                          output_type_id))
  df_sample_id <- dplyr::filter(df, output_type == "sample")
  if (length(unique(df_sample_id$output_type_id)) <= 1) {
    add_message <- paste0(add_message,
                          "\n\U000274c Error 902: The submission should ",
                          "contains multiple sample output type groups, ",
                          "please verify.\n")
  }
  df <- dplyr::select(df, -run_grouping, -stochastic_run)
  return(list("df" = df,
              "add_message" = add_message))
}

#' Create the validation report
#'
#' Combine all the test output function into one report
#'
#' @param df data frame to test
#' @param model_task list containing round definitions: names of columns,
#' target names, ...
#' @param col_message character vector, error message about the columns names
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
#' @param out_sample character vector, error message about `"sample"` output
#' type to append to the report. Uses only if the submission is expected to
#' contains `"sample"` values
#' @param out_quant character vector, error message about `"quantile"` output
#' type to append to the report. Uses only if the submission is expected to
#' contains `"quantile"` values
#' @param out_agegroup character vector, error message about `age_group`
#' column to append to the report. Uses only if the submission is expected to
#' contains a `age_group` column
#' @param out_race_ethnicity character vector, error message about
#' `race_ethnicity` column to append to the report. Uses only if the submission
#' is expected to contains a `race_ethnicity` column
#' @param add_meesage character vector, error message to append to the report
#'
#' @noRd
create_report <- function(df, model_task, col_message, out_col, out_scen,
                          out_ord, out_val, out_target, out_loc, out_sample,
                          out_quant, out_agegroup, out_raceethnicity,
                          add_message = NULL) {
  test_report <-
    paste("\n ## Columns: \n", paste(out_col, col_message, collapse = "\n"),
          "\n\n## Scenarios: \n", paste(out_scen, collapse = "\n"),
          "\n\n## Origin Date Column:  \n", paste(out_ord, collapse = "\n"),
          "\n\n## Value and Type Columns: \n", paste(out_val, collapse = "\n"),
          "\n\n## Target Columns: \n", paste(out_target, collapse = "\n"),
          "\n\n## Locations: \n", paste(out_loc, collapse = "\n"))
  if (any(grepl("sample", unlist(distinct(df[, "output_type", FALSE])))) ||
        any("sample" %in% names(unlist(purrr::map(model_task, "output_type"),
                                       FALSE))))  {
    if (!is.null(add_message)) {
      if (any(grepl("No errors or warnings", out_sample))) {
        test_report <- paste(test_report, "\n\n## Sample: \n",
                             paste(add_message, collapse = "\n"))
      } else { # nocov start
        test_report <- paste(test_report, "\n\n## Sample: \n",
                             paste(add_message, out_sample, collapse = "\n"))
      } # nocov end
    } else {
      test_report <- paste(test_report, "\n\n## Sample: \n",
                           paste(out_sample, collapse = "\n"))
    }

  }
  if (any(grepl("quantile", unlist(distinct(df[, "output_type", FALSE])))) ||
        any("quantile" %in% names(unlist(purrr::map(model_task, "output_type"),
                                         FALSE))))  {
    test_report <- paste(test_report, "\n\n## Quantiles: \n",
                         paste(out_quant, collapse = "\n"))
  }
  if (any(grepl("age_group", names(df)))) {
    test_report <- paste(test_report, "\n\n## Age Group: \n",
                         paste(out_agegroup, collapse = "\n"))
  }
  if (any(grepl("race_ethnicity", names(df)))) {
    test_report <- paste(test_report, "\n\n## Race Ethnicity: \n",
                         paste(out_raceethnicity, collapse = "\n"))
  }

  test_report <- paste0(test_report, "\n\n")
  return(test_report)
}

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
#'@param last_lst_gs list of data frame, named with the corresponding target and
#'  containing the last avaible week of observed data  before start of the
#'  projection
#'@param js_def list containing round definitions: names of columns,
#' target names, ...
#'@param merge_sample_col boolean to indicate if the for the output type
#' "sample", the output_type_id column is set to NA and the information is
#' contained into 2 columns: "run_grouping" and "stochastic_run". By default,
#' `FALSE`
#'@param pairing_col column names indicating the sample pairing information. By
#' default: "horizon".
#'@param n_decimal integer,  number of decimal point accepted in the column
#'  value (only for "sample" output type), if NULL (default) no limit expected.
#'
#'@details Internal function called in the `validation_submission()` function.
#' For more information on all tests run on the submission, please refer to the
#' documentation of each "test_*" function. A vignette with all the information
#' might be created later on too.
#'
#' @noRd
run_all_validation <- function(df, path, pop, last_lst_gs, js_def,
                               merge_sample_col = FALSE,
                               pairing_col = "horizon", n_decimal = NULL) {
  ### Prerequisite
  model_task <- js_def$model_tasks
  task_ids <- purrr::map(model_task, "task_ids")
  req_colnames <-  c(unique(names(unlist(task_ids, FALSE))),
                     "output_type", "output_type_id", "value")

  # Test if any factor columns
  if (any(sapply(colnames(df), function(x) is.factor(df[[x]])))) {
    col_message <- paste0("\n\U000274c Error 104: At least one column is in a ",
                          "format: 'factor', please verify")
  } else {
    col_message <- NULL
  }
  df <- dplyr::mutate_if(df, is.factor, as.character)

  # Merge sample ID column
  if (isTRUE(merge_sample_col)) {
    sample_update <- merge_sample_id(df, req_colnames, add_message = NULL)
    df <- sample_update[["df"]]
    add_message <- sample_update[["add_message"]]
  } else {
    add_message <- NULL
  }

  ### Tests:
  # Test on column information (name and number)
  out_col <- test_column(df, req_colnames)

  # select only required column for the other tests
  df <- df[, req_colnames]

  # Test on Scenario information
  out_scen <- test_scenario(df, model_task)

  # Test origin date information
  date_id <- unique(unlist(purrr::map(purrr::map(model_task, "task_ids"),
                                      "origin_date")))
  out_ord <- test_origindate(df, path, id = date_id)

  # Test by type
  if (any(grepl("quantile", unlist(distinct(df[, "output_type", FALSE])))) ||
        any("quantile" %in% names(unlist(purrr::map(model_task, "output_type"),
                                         FALSE)))) {
    out_quant <- test_quantiles(df, model_task)
  }
  if (any(grepl("sample", unlist(distinct(df[, "output_type", FALSE])))) ||
        any("sample" %in% names(unlist(purrr::map(model_task, "output_type"),
                                       FALSE)))) {
    out_sample <- test_sample(df, model_task, pairing_col = pairing_col)
  }

  # Test on value
  out_val <- test_val(df, pop, last_lst_gs, model_task, n_decimal = n_decimal)

  # Test on targets information
  out_target <- test_target(df, model_task)

  # Test on location information
  out_loc <- test_location(df, model_task)

  # Test for additional column
  if (any(grepl("age_group", names(df)))) {
    out_agegroup <- test_agegroup(df, model_task)
  }

  # Test for additional column
  if (any(grepl("race_ethnicity", names(df)))) {
    out_raceethnicity <- test_raceethnicity(df, model_task)
  }

  # Report:
  test_report <- create_report(df, model_task, col_message, out_col, out_scen,
                               out_ord, out_val, out_target, out_loc,
                               out_sample, out_quant, out_agegroup,
                               out_raceethnicity, add_message = add_message)

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
#' `test_scenario()`, `test_origindate()`, `test_quantiles()`, `test_val()`,
#' `test_target()`, `test_location()`,` test_sample()`, `test_agegroup()`,
#' `test_raceethnicity()`) on a Scenario Modeling Hub (SMH)
#' submissions and prints information about the results of each tests on the
#' submission: warning(s), error(s) or message if all the tests were successful.
#'
#'@param path path to the submissions file (or folder for partitioned data)
#' to test, or string of parquet files (in this case, the validation will be
#' run on the aggregation of all the parquet files together, and each file
#' individually should match the expected SMH standard).
#' If partition is not set to `NULL`, path to the folder containing the
#' partitioned data.
#'@param js_def path to JSON file containing round definitions: names of
#' columns, target names, ... following the `tasks.json`
#' [Hubverse
#' ](https://hubdocs.readthedocs.io/en/latest/user-guide/hub-config.html)
#' format
#'@param pop_path path to a table containing the population size of each
#'  geographical entities by FIPS (in a column "location") and by location name.
#'  Use to compare that value is not higher than expected population size.
#'  Set to `NULL` (default), to NOT run comparison with observed data.
#'@param lst_gs named list of data frame containing the
#' observed data. For COVID-19, we highly recommend to use the output of the
#' pull_gs_data() function. The list should have the same format: each data
#' frame should be named with the corresponding covidcast signal except
#' "hospitalization" instead of "confirmed_admissions_covid_1d".
#' Set to `NULL` (default), to NOT run comparison with observed data.
#'@param merge_sample_col boolean to indicate if the for the output type
#' "sample", the output_type_id column is set to NA and the information is
#' contained into 2 columns: "run_grouping" and "stochastic_run". By default,
#' `FALSE`
#'@param partition vector, for csv and parquet files, allow to validate files
#' in a partition format, see `arrow` package for more information, and
#' `arrow::write_dataset()`, `arrow::open_dataset()` functions.
#'@param n_decimal integer,  number of decimal point accepted in the column
#'  value (only for "sample" output type), if NULL (default) no limit expected.
#'@param round_id character string, round identifier. If `NULL` (default),
#' extracted from `path`.
#'
#'@details For more information on all tests run on the submission, please refer
#' to the documentation of each `"test_*`" function. A vignette with all the
#' information is available in the package and is called:
#' vignette("validation-checks").
#'
#' For the `"location"` column, by default the SMHvalidation package will
#' validate it to the con
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
#' @importFrom arrow open_dataset Field int64 schema
#' @importFrom purrr list_simplify
#' @importFrom hubData create_hub_schema
#' @importFrom hubUtils get_round_ids
#'
#'@examples
#' \dontrun{
#'
#' }
#'@export
validate_submission <- function(path, js_def, lst_gs = NULL, pop_path = NULL,
                                merge_sample_col = FALSE, partition = NULL,
                                n_decimal = NULL, round_id = NULL) {

  # Prerequisite --------
  # Load gold standard data
  lst_gs <- lapply(seq_along(lst_gs), function(x) {
    df_gs <- dplyr::mutate(lst_gs[[x]],
                           target_name = names(lst_gs[x])) %>%
      dplyr::select(time_value, value, location = fips, target_name)
    return(df_gs)
  }) %>%
    setNames(names(lst_gs))

  # Pull population data and prepare location hash vector
  if (!is.null(pop_path)) pop <- read_files(pop_path)

  # Read JSON file
  js_def0 <- jsonlite::fromJSON(js_def, simplifyDataFrame = FALSE)
  js_date <- unique(as.Date(hubUtils::get_round_ids(js_def0)))
  # Select the associated round (add error message if no match)
  if (is.null(round_id)) {
    if (!is.null(partition)) {
      team_round <- as.Date(stringr::str_extract(dir(path, recursive = TRUE),
                                                 "\\d{4}-\\d{2}-\\d{2}"))
    } else {
      team_round <- as.Date(stringr::str_extract(path, "\\d{4}-\\d{2}-\\d{2}"))
    }
    round_id <- unique(team_round)
  }
  if (as.Date(round_id) %in% js_date) {
    js_def <- js_def0$rounds[js_date %in% as.Date(round_id)]
    js_def <- js_def[[1]]
  } else {
    err004 <-
      paste0("\U000274c Error 004: The origin_date in the submission file was",
             " not associated with any task_ids round. Please verify the date",
             " information in the origin_date column corresponds to the ",
             "expected value.\n")
    cat(err004)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }

  # Select validation file(s) and print message --------
  if (!is.null(partition)) {
    file_path <- grep(round_id, dir(path, recursive = TRUE), value = TRUE)
  } else {
    file_path <- basename(path)
  }
  if (length(file_path) > 10) { # nocov start
    cat(paste0("Run validation on files: ",
               paste(unique(file_path)[1:5], collapse = ", "),
               ", etc.\n"))
  } else { # nocov end
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
    df <- dplyr::collect(ds) %>%
      dplyr::mutate_if(is.factor, as.character)
  } else if (!is.null(partition)) {
    df <- load_partition_arrow(path, js_def = js_def0, js_def_round = js_def,
                               partition = partition,
                               merge_sample_col = merge_sample_col)
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

  # test date format
  df <- as.data.frame(df)
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

  sample_js <- unique(purrr::map(purrr::map(js_def$model_tasks,
                                            "output_type"), "sample"))
  if (!all(is.null(sample_js))) {
    sample_list <- purrr::map(sample_js, "output_type_id")
    if (!all(is.null(unique(unlist(purrr::map(sample_list,
                                              "samples_joint_across")))))) {
      pairing_col <- unique(unlist(purrr::map(sample_list,
                                              "samples_joint_across")))
    } else {
      pairing_col <- "horizon"
    }
  }
  # Extract week 0 or week -1 of observed data
  last_week_gs <-  lapply(lst_gs, function(x) {
    lastw_df <- dplyr::filter(x, time_value < as.Date(js_def$round_id) + 6) %>%
      dplyr::filter(time_value == max(time_value)) %>%
      dplyr::select(last_value = value, location, target_name)
    return(lastw_df)
  })

  # Run tests --------
  run_all_validation(df, path = path, pop = pop, last_lst_gs = last_week_gs,
                     js_def = js_def, merge_sample_col = merge_sample_col,
                     pairing_col = pairing_col, n_decimal = n_decimal)
}
