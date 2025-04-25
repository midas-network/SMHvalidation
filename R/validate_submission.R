#' Run all validation checks and output a report
#'
#' Runs all the different validation checks functions on a data frame and print
#' information about the results of each tests on the submission: warning(s),
#' error(s) or if all the tests were successful.
#' Internal function called in the `validation_submission()` function.
#' For more information on all tests run on the submission, please refer to the
#' documentation of each "test_*" function. A vignette with all the information
#' might be created later on too.
#'
#' @importFrom hubValidations new_hub_validations parse_file_name try_check
#' check_tbl_colnames check_tbl_col_types
#' check_tbl_value_col check_tbl_value_col_ascending
#' @importFrom dplyr mutate_all
#'
#' @noRd
run_all_validation <- function(df, path, js_def0, js_def, round_id, hub_path,
                               pop = NULL, obs = NULL, merge_sample_col = NULL,
                               partition = NULL, n_decimal = NULL,
                               verbose = TRUE, verbose_col =  NULL) {
  ### Prerequisite
  req_colnames <-  c(get_tasksids_colnames(js_def), "output_type",
                     "output_type_id", "value")
  checks <- new_hub_validations()
  if (!is.null(partition)) {
    file_path <- unique(basename(dir(path, recursive = TRUE)))
  } else {
    file_path <- path
  }

  # Merge sample ID column
  if (!is.null(merge_sample_col)) {
    # Validation
    if (!(all(c(req_colnames, merge_sample_col) %in% names(df)))) {
      fail_col <- req_colnames[!req_colnames %in% names(df)]
      details_mess <- paste0("'", paste(fail_col, collapse = "', '"),
                             "' should be present in the file.")
      mgs_attr <- paste0("consistent with expected round task IDs and std ",
                         "column names.")
      checks$col_names <-
        try_check(capture_check_cnd(!length(fail_col) > 0, file_path,
                                    msg_subject = "Column names",
                                    msg_attribute = mgs_attr,
                                    details = details_mess, error = TRUE))
      return(checks)
    }
    all <- merge_sample_id(df, req_colnames, merge_sample_col,  js_def0, js_def,
                           checks, partition = partition, verbose = verbose,
                           verbose_col = verbose_col)
    df <- all$df
    msg <- all$msg
    if (!is.null(msg))
      checks$pairing_info <- capture_check_info(file_path = file_path,
                                                msg = msg)
  }

  checks <- round_id_test(checks, df, file_path, hub_path, path)
  if (is_any_error(checks$match_round_id) ||
        is_any_error(checks$unique_round_id)) {
    return(checks)
  }

  checks$colnames <-
    try_check(check_tbl_colnames(df, round_id = round_id,
                                 file_path = file_path,
                                 hub_path = hub_path), path)
  if (is_any_error(checks$colnames)) {
    return(checks)
  }

  checks$col_types <-
    try_check(check_tbl_col_types(df, file_path = file_path,
                                  hub_path = hub_path,
                                  output_type_id_datatype = "from_config"),
              path)
  tbl_chr <- dplyr::mutate_all(df, as.character)

  checks$valid_vals <-
    try_check(check_tbl_values(tbl_chr, round_id = round_id,
                               file_path = file_path, hub_path = hub_path),
              path)
  if (is_any_error(checks$valid_vals)) {
    return(checks)
  }

  checks$rows_unique <-
    try_check(check_tbl_rows_unique(tbl_chr, file_path = file_path,
                                    hub_path = hub_path), path)

  checks$req_vals <-
    try_check(check_df_values_required(df, js_def, file_path = file_path), path)
  # -- slow
  checks$value_col_valid <-
    try_check(check_tbl_value_col(df, round_id = round_id,
                                  file_path = file_path,
                                  hub_path = hub_path, derived_task_ids = NULL),
              path)

  checks$value_col_non_desc <-
    try_check(check_tbl_value_col_ascending(tbl_chr, file_path = file_path,
                                            hub_path = hub_path,
                                            round_id = round_id,
                                            derived_task_ids = NULL), path)
  # -- slow
  checks <- sample_test(checks, tbl_chr, round_id, file_path, hub_path,
                        path)
  checks <- value_test(df, checks, file_path, n_decimal = n_decimal, pop = pop,
                       obs = obs)

  return(checks)
}


#' Validate SMH (Scenario Modeling Hub) Submissions
#'
#' Runs multiple validation checks functions on SMH submission
#' file(s) and prints information about the results of each test on the
#' submission file(s): warning(s), error(s) and/or message in the hubverse
#' format.
#'
#'@param path path to the submissions file (or folder for partitioned data)
#' to test, or string of parquet files (in this case, the validation will be
#' run on the aggregation of all the parquet files together, and each file
#' individually should match the expected SMH standard). The path should be
#' relative to the `hub_path`, model output folder.
#' If partition is not set to `NULL`, path to the folder containing the
#' partitioned data.If the path contains multiple rounds information, please
#' use the `round_id` parameter to indicate which round to test.
#' @param hub_path path to the repository contains the submissions files
#' and `tasks.json` file.
#' @param js_def path to a JSON file containing round definitions: names of
#' columns, target names, ... following the `tasks.json`
#' [Hubverse](https://hubverse.io/en/latest/user-guide/hub-config.html)
#' format. If `NULL` (default), will use the default path
#' "hub-config/tasks.json" in the hub using `hub_path` parameter.
#'@param target_data path to the target data in the Hubverse time series target
#' data standard format. Please find additional information on the
#' [hubverse](https://hubverse.io/en/latest/user-guide/target-data.html)
#' website. Set to `NULL` (default), to NOT include comparison with observed
#' data.
#'@param pop_path path to a table containing the population size of each
#' geographical entities by FIPS (in a column `"location"`).
#' Use to compare that value is not higher than expected population size.
#' Set to `NULL` (default), to NOT run comparison with population data.
#'@param merge_sample_col vector to indicate if the for the output type
#' "sample", the output_type_id column is set to NA and the information is
#' contained into other columns. If so the parameter should be set to the sample
#' ID columns names, for example: `c("run_grouping" and "stochastic_run")`.
#' By default, `NULL` (sample ID information in the output type id column).
#'@param partition vector, for csv and parquet files, allow to validate files
#' in a partition format, see `arrow` package for more information, and
#' `arrow::write_dataset()`, `arrow::open_dataset()` functions.  By default
#' `NULL`, no partition.
#'@param n_decimal integer, number of decimal points accepted in the column
#' value (only for `"sample"` output type), if `NULL` (default) no limit
#' expected.
#'@param round_id character string, round identifier. If `NULL` (default),
#' extracted from `path`.
#'@param verbose Boolean, if TRUE add information about the sample pairing
#'  information in output message. By default, `TRUE` (slows the validation
#'  validation for sample output type)
#'@param verbose_col character vector, details of values included in the pairing
#' of each group for specific mentioned columns. If `NULL` (default), no
#' additional information in the pairing information. (require `verbose` set
#' to `TRUE`).
#'@param r_schema Schema arrow objects, to read partition files with a specific
#' schema. If none provided (default, `NULL`), the schema will be created from
#' the `js_def` JSON file. (only for partition files).
#'
#'@details For more information on all tests run on the submission, please refer
#' to the documentation of the
#' [hubValidations](https://hubverse-org.github.io/hubValidations/index.html)
#' package. A vignette with all the information is available in the package and
#' is called: vignette("validation-checks").
#'
#' The function accepts submission in PARQUET, CSV, ZIP or GZ file formats. If
#' the submission files is in a "partitioned" format, the `path` parameter
#' should be to a directory to a folder containing ONLY the "partitioned"
#' files. If any other file is present in the directory, it will be included
#' in the validation.
#'
#' For partitioned files, if the `path` contains multiple rounds, please use
#' the `round_id` parameter to signal which round to test.
#'
#' The function runs some preliminary tests before running all the checks:
#' * Input submission file format: The file format of the submission
#'  file(s) corresponds to the expected format (for example: `parquet`, or
#'  `csv`, etc.). If multiple files inputted, only `parquet` is accepted.
#'  * Date information: The column `origin_date` in the submission file
#'   corresponds to a `model_tasks` round information in the JSON file
#'   (`js_def` parameter).
#'  * Date format: All columns containing dates information should be in
#'   "YYY-MM-DD" format.
#' * Location name: The submission should contain projection by
#'  location, the `location` column contains the location FIPS number as
#'  available in the location table in the SMH GitHub Repository. If the FIPS
#'  number are missing a trailing zero, the submission will be accepted but a
#'  warning message will be returned.
#' * Column format: If the submission file(s) contain column in a factor column,
#' the column will be forced to character, and a warning message will be
#' returned
#'
#' @importFrom hubUtils get_round_ids read_config_file get_round_model_tasks
#' @importFrom arrow open_dataset
#' @importFrom dplyr collect mutate_if mutate_all distinct across
#' @importFrom tidyr all_of
#'
#'@examples
#' hub_path <- system.file("extdata/exp/", package = "SMHvalidation")
#' sub_path <- "team2-modelb/2023-11-12-team2-modelb.parquet"
#' validate_submission(sub_path, hub_path)
#'
#' # Partitioned Files
#' path_2 <- "t3-mc/"
#' validate_submission(path_2, hub_path, partition = c("origin_date", "target"),
#'                     merge_sample_col = c("run_grouping", "stochastic_run"),
#'                     round_id = "2023-11-12")
#'
#'@export
validate_submission <- function(path, hub_path, js_def = NULL,
                                target_data = NULL, pop_path = NULL,
                                merge_sample_col = NULL, partition = NULL,
                                n_decimal = NULL, round_id = NULL,
                                verbose = TRUE, verbose_col = NULL,
                                r_schema = NULL) {

  # Prerequisite --------
  m_fold <- hubUtils::read_config(hub_path, "admin")$model_output_dir
  path <- paste0(m_fold, "/", path)
  # Pull target data
  if (!is.null(target_data)) obs <- read_files(target_data) else obs <- NULL
  # Pull population data
  if (!is.null(pop_path)) pop <- read_files(pop_path) else pop <- NULL
  # Select the associated round (add error message if no match)
  if (is.null(round_id))
    round_id <- team_round_id(paste0(hub_path, "/", path),
                              partition = partition)

  check <- new_hub_validations()

  # Select validation file(s) and print message --------
  file_path <- file_path_info(path, hub_path, partition = partition,
                              round_id = round_id, verbose = verbose)

  # Read hub config JSON file ------
  if (is.null(js_def)) js_def <- paste0(hub_path, "/hub-config/tasks.json")
  js_def0 <- hubUtils::read_config_file(js_def)
  check_round_id <- round_id %in% hubUtils::get_round_ids(js_def0)
  details_mess <-
    paste0("The round id (date in the submission file or parameters) is not ",
           "associated with any task_ids round. Please verify the date ",
           "information in 'path' or in the 'round_id' parameter.\nPossible ",
           "round ids are: ", paste(unique(hubUtils::get_round_ids(js_def0)),
                                    collapse = ", "))
  check$round_id <-
    try_check(capture_check_cnd(check_round_id, file_path,
                                msg_subject = "{.var round_id}",
                                msg_attribute = "valid.",
                                etails = details_mess, error = TRUE))
  if (is_any_error(check$round_id)) {
    return(check)
  } else {
    js_def <- hubUtils::get_round_model_tasks(js_def0, as.character(round_id))
  }

  # Process file to test and associated information --------
  sub_file_ext <- unique(sub(".*\\.", "", file_path))
  config_ext <- hubValidations::read_config(hub_path, "admin")$file_format
  check_file_ext <- sub_file_ext %in% config_ext
  details_mess <- paste0("Extension(s) accepted: {.val ", config_ext, "}")
  check$file_extension <-
    try_check(capture_check_cnd(check_file_ext, file_path,
                                msg_subject = "File(s) format extension",
                                msg_attribute = "valid.",
                                details = details_mess, error = TRUE))
  if (is_any_error(check$file_extension)) {
    return(check)
  } else {
    # Read file
    if (length(file_path) == 1) {
      df <- read_files(paste0(hub_path, "/", path))
    } else if (!is.null(partition)) {
      schema <- make_schema(js_def0, js_def, round_id,
                            path = paste0(hub_path, "/", path),
                            merge_sample_col = merge_sample_col,
                            r_schema = r_schema, partition = partition)
      df <- load_partition_arrow(paste0(hub_path, "/", path),
                                 partition = partition,
                                 schema = schema)
    }
  }

  # test date format
  test_date <- df[, grepl("date", names(df)), FALSE]
  test_date <- unlist(dplyr::mutate_all(dplyr::distinct(test_date),
                                        as.character))
  check_test_format <- !any(is.na(as.Date(na.omit(test_date), "%Y-%m-%d")))
  details_mess <- "The column should be in a ISO date format {.val YYYY-MM-DD}"
  message <- "The column(s) containing date information"
  check$date_format <-
    try_check(capture_check_cnd(check_test_format, file_path,
                                msg_subject = message,
                                msg_attribute = "in a valid format.",
                                details = details_mess, error = TRUE))
  if (is_any_error(check$date_format)) {
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
      dplyr::select(tidyr::all_of(c("observation", "location", "target",
                                    "age_group")))
  }

  # Run tests --------
  run_all_validation(df, paste0(hub_path, "/", path), js_def0, js_def, round_id,
                     hub_path, pop = pop, obs = obs, verbose = verbose,
                     merge_sample_col = merge_sample_col, n_decimal = n_decimal,
                     partition = partition, verbose_col = verbose_col)
}
