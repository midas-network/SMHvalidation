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
#' @importFrom hubValidations new_hub_validations parse_file_name try_check
#' check_valid_round_id_col check_tbl_unique_round_id is_any_error
#' check_tbl_match_round_id check_tbl_colnames check_tbl_col_types
#' check_tbl_values check_tbl_rows_unique check_tbl_values_required
#' check_tbl_value_col check_tbl_value_col_ascending
#' check_tbl_spl_compound_taskid_set check_tbl_spl_compound_tid
#' check_tbl_spl_non_compound_tid check_tbl_spl_n
#'
#' @importFrom hubData coerce_to_character
#'
#' @noRd
run_all_validation <- function(df, path, js_def0, js_def, round_id, hub_path,
                               pop = NULL, obs = NULL, merge_sample_col = NULL,
                               partition = NULL, n_decimal = NULL,
                               verbose = TRUE) {
  ### Prerequisite
  req_colnames <-  c(get_tasksids_colnames(js_def), "output_type",
                     "output_type_id", "value")

  # Merge sample ID column
  if (!is.null(merge_sample_col)) {
    df <- merge_sample_id(df, req_colnames, merge_sample_col,  js_def0, js_def,
                          partition = partition, verbose = verbose)
  }

  checks <- new_hub_validations()

  checks$valid_round_id_col <-
    try_check(check_valid_round_id_col(df, file_path = path,
                                       hub_path = hub_path), path)
  checks$unique_round_id <-
    try_check(check_tbl_unique_round_id(df, file_path = path,
                                        hub_path = hub_path), path)
  if (is_any_error(checks$unique_round_id)) {
    return(checks)
  }

  checks$match_round_id <-
    try_check(check_tbl_match_round_id(df, file_path = path,
                                       hub_path = hub_path), path)
  if (is_any_error(checks$match_round_id)) {
    return(checks)
  }

  checks$colnames <-
    try_check(check_tbl_colnames(df, round_id = round_id, file_path = path,
                                 hub_path = hub_path), path)
  if (is_any_error(checks$colnames)) {
    return(checks)
  }

  checks$col_types <-
    try_check(check_tbl_col_types(df, file_path = path, hub_path = hub_path,
                                  output_type_id_datatype = "from_config"),
              path)
  tbl_chr <- hubData::coerce_to_character(df)

  checks$valid_vals <-
    try_check(check_tbl_values(tbl_chr, round_id = round_id, file_path = path,
                               hub_path = hub_path, derived_task_ids = NULL),
              path)
  if (is_any_error(checks$valid_vals)) {
    return(checks)
  }

  checks$rows_unique <-
    try_check(check_tbl_rows_unique(tbl_chr, file_path = path,
                                    hub_path = hub_path), path)
  checks$req_vals <-
    try_check(check_tbl_values_required(tbl_chr, round_id = round_id,
                                        file_path = path, hub_path = hub_path,
                                        derived_task_ids = NULL), path)
  # -- slow
  checks$value_col_valid <-
    try_check(check_tbl_value_col(df, round_id = round_id, file_path = path,
                                  hub_path = hub_path, derived_task_ids = NULL),
              path)

  checks$value_col_non_desc <-
    try_check(check_tbl_value_col_ascending(tbl_chr, file_path = path,
                                            hub_path = hub_path,
                                            round_id = round_id,
                                            derived_task_ids = NULL), path)

  checks$spl_compound_taskid_set <-
    try_check(check_tbl_spl_compound_taskid_set(tbl_chr, round_id = round_id,
                                                file_path = path,
                                                hub_path = hub_path,
                                                derived_task_ids = NULL), path)
  cmd_tkid_set <- checks$spl_compound_taskid_set$compound_taskid_set
  checks$spl_compound_tid <-
    try_check(check_tbl_spl_compound_tid(tbl_chr, round_id = round_id,
                                         file_path = path, hub_path = hub_path,
                                         compound_taskid_set = cmd_tkid_set,
                                         derived_task_ids = NULL), path)
  if (is_any_error(checks$spl_compound_tid)) {
    return(checks)
  }
  checks$spl_non_compound_tid <-
    try_check(check_tbl_spl_non_compound_tid(tbl_chr, round_id = round_id,
                                             file_path = path,
                                             hub_path = hub_path,
                                             compound_taskid_set = cmd_tkid_set,
                                             derived_task_ids = NULL), path)
  if (is_any_error(checks$spl_non_compound_tid)) {
    return(checks)
  }
  checks$spl_n <-
    try_check(check_tbl_spl_n(tbl_chr, round_id = round_id, file_path = path,
                              hub_path = hub_path,
                              compound_taskid_set = cmd_tkid_set,
                              derived_task_ids = NULL), path)

  return(checks)
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
#' @param hub_path path to the repository contains the submissions files
#' and js_def file
#'@param target_data data frame containing the
#' observed data. We highly recommend to use the output of the
#' `pull_gs_data()` function. The data frame should have the time
#' series hubverse format.
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
#' * Location name: The submission should contains projection by
#'  location, the `location` column contains the location FIPS number as
#'  available in the location table in the SMH GitHub Repository. If the FIPS
#'  number are missing a trailing zero, the submission will be accepted but a
#'  warning message will be returned.
#'
#' @importFrom hubUtils get_round_ids read_config_file get_round_model_tasks
#' @importFrom stringr str_extract
#' @importFrom arrow open_dataset
#' @importFrom dplyr collect mutate_if mutate_all distinct across
#' @importFrom tidyr all_of
#'
#'@examples
#' \dontrun{
#' validate_submission("PATH/TO/SUBMISSION", "PATH/TO/ROUND/tasks.json")
#' }
#'@export
validate_submission <- function(path, js_def, hub_path, target_data = NULL,
                                pop_path = NULL, merge_sample_col = NULL,
                                partition = NULL, n_decimal = NULL,
                                round_id = NULL, verbose = TRUE,
                                r_schema = NULL) {

  # Prerequisite --------
  # Pull target data
  if (!is.null(target_data)) obs <- read_files(target_data) else obs <- NULL
  # Pull population data
  if (!is.null(pop_path)) pop <- read_files(pop_path) else pop <- NULL
  # Select the associated round (add error message if no match)
  if (is.null(round_id)) round_id <- team_round_id(path, partition = partition)

  # Select validation file(s) and print message --------
  if (!is.null(partition)) {
    file_path <- grep(round_id, dir(path, recursive = TRUE), value = TRUE) |>
      unique()
    file_path_mess <- c(file_path[1:min(max(length(file_path) - 1, 1), 5)],
                        "etc.")
    cat(paste0("Run validation on files: ",
               paste(file_path_mess, collapse = ", "), "\n"))
  } else {
    file_path <- basename(path)
    cat(paste0("Run validation on files: ", paste(unique(file_path),
                                                  collapse = ", "), "\n"))
  }

  # Read hub config JSON file ------
  js_def0 <- hubUtils::read_config_file(js_def)
  check_round_id <- round_id %in% hubUtils::get_round_ids(js_def0)
  details_mess <-
    paste0("The round id (date in the submission file or parameters) is not ",
           "associated with any task_ids round. Please verify the date ",
           "information in 'path' or in the 'round_id' parameter.\nPossible ",
           "round ids are: ", paste(unique(hubUtils::get_round_ids(js_def0)),
                                    collapse = ", "))
  check <-
    hubValidations::capture_check_cnd(check_round_id, file_path,
                                      msg_subject = "{.var round_id}",
                                      msg_attribute = "valid.",
                                      details = details_mess, error = TRUE)
  if (hubValidations::is_any_error(check)) {
    return(check)
  } else {
    js_def <- hubUtils::get_round_model_tasks(js_def0, as.character(round_id))
  }

  # Process file to test and associated information --------
  sub_file_ext <- unique(sub(".*\\.", "", file_path))
  config_ext <- hubValidations::read_config(hub_path, "admin")$file_format
  check_file_ext <- sub_file_ext %in% config_ext
  details_mess <- paste0("Extension(s) accepted: {.val ", config_ext, "}")
  check <-
    hubValidations::capture_check_cnd(check_file_ext, file_path,
                                      msg_subject = "File(s) format extension",
                                      msg_attribute = "valid.",
                                      details = details_mess, error = TRUE)
  if (hubValidations::is_any_error(check)) {
    return(check)
  } else {
    # Read file
    if (length(path) == 1) {
      df <- read_files(path)
    } else if (!is.null(partition)) {
      schema <- make_schema(js_def0, js_def, round_id, path = path,
                            merge_sample_col = merge_sample_col,
                            r_schema = r_schema, partition = partition)
      df <- load_partition_arrow(path, partition = partition, schema = schema)
    }
  }

  # test date format
  test_date <- df[, grepl("date", names(df)), FALSE]
  test_date <- unlist(dplyr::mutate_all(dplyr::distinct(test_date),
                                        as.character))
  check_test_format <- !any(is.na(as.Date(na.omit(test_date), "%Y-%m-%d")))
  details_mess <- "The column should be in a ISO date format {.val YYYY-MM-DD}"
  message <- "The column(s) containing date information"
  check <-
    hubValidations::capture_check_cnd(check_test_format, file_path,
                                      msg_subject = message,
                                      msg_attribute = "in a valid format.",
                                      details = details_mess, error = TRUE)
  if (hubValidations::is_any_error(check)) {
    return(check)
  } else {
    col_date <- grep("date", names(df), value = TRUE)
    df <- dplyr::mutate(df, dplyr::across(tidyr::all_of(col_date), as.Date))
  }

  # Extract week 0 or week -1 of observed data
  or_date <- unique(unlist(purrr::map_depth(js_def, 2, "origin_date")))
  if (!is.null(obs)) {
    obs <- dplyr::filter(obs, date < as.Date(or_date) + 6) |>
      dplyr::filter(date == max(date)) |>
      dplyr::select(tidyr::all_of(c("observation", "location", "signal")))
  }

  # Run tests --------
  run_all_validation(df, path, js_def0, js_def, round_id, hub_path, pop = pop,
                     obs = obs, merge_sample_col = merge_sample_col,
                     n_decimal = n_decimal, verbose = verbose,
                     partition = partition)
}
