# extract tasks ids column names
get_tasksids_colnames <- function(js_def) {
  taskids_col <- purrr::map(js_def, "task_ids") |>
    purrr::flatten() |>
    names() |>
    unique()
  return(taskids_col)
}

# extract team round id information
team_round_id <- function(path, partition = NULL) {
  # Select the associated round (add error message if no match)
  date_pttrn <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
  if (!is.null(partition)) {
    team_round <- dir(path, recursive = TRUE)
    team_round <- team_round[regexpr(date_pttrn, team_round) > 0]
    team_round <- substring(team_round, regexpr(date_pttrn, team_round),
                            regexpr(date_pttrn, team_round) + 9)
  } else {
    team_round <- gsub(paste0("(?<=", date_pttrn, ").+"), "", basename(path),
                       perl = TRUE)
  }
  round_id <- unique(team_round)
  return(round_id)
}

# Test of any factor columns
factor_columns <- function(df) {
  if (any(sapply(colnames(df), function(x) is.factor(df[[x]])))) {
    warning("\U0001f7e1 Warning: At least one column is in a format: 'factor',",
            " please verify. \n The column(s) will be automatically set to ",
            " 'character'.")
  }
  df <- dplyr::mutate_if(df, is.factor, as.character)
  return(df)
}

# Identify file format extension - Partitioned format
id_file_format <- function(path) {
  err005 <- paste0("\U000274c 005: The file format of the submission was not",
                   " recognized, please use one unique file or multiple ",
                   "parquet files. For more information, please look at the ",
                   "documentation of the hub. \n")
  filef <-
    dplyr::case_when(all(grepl("parquet$|.pqt$",
                               dir(path, recursive = TRUE))) ~ "parquet",
                     all(grepl(".csv$",
                               dir(path, recursive = TRUE))) ~ "csv",
                     .default = err005)
  if (grepl("\U000274c", filef)) {
    stop(err005)
  }
  return(filef)
}

# Create schema for loading files using arrow
# optional testing of the column names matching the schema
make_schema <- function(js_def, js_def_round, round_id, path = NULL,
                        merge_sample_col = NULL, r_schema = NULL,
                        partition = NULL) {
  exp_col <- c(unique(names(unlist(purrr::map(js_def_round, "task_ids"),
                                   FALSE))),
               "output_type", "output_type_id", "value")
  if (is.null(r_schema)) {
    schema <- hubData::create_hub_schema(js_def)
    if (!is.null(merge_sample_col)) {
      exp_col <- c(exp_col, merge_sample_col)
      schema <-
        c(schema$fields,
          purrr::map(merge_sample_col,
                     function(x) arrow::Field$create(x, arrow::int64())))
      schema <- arrow::schema(schema)
    }
  } else {
    exp_col <- names(r_schema)
    schema <- r_schema
  }
  if (!is.null(path)) {
    filef <- id_file_format(path)
    files_path <- grep(round_id,
                       dir(path, full.names = TRUE,
                           recursive = TRUE), value = TRUE)
    col_names <- arrow::open_dataset(sources = files_path,
                                     format = filef)$schema$names
    col_names <- unique(c(col_names, partition))
    if (!(all(exp_col %in% col_names)) || !(all(col_names %in% exp_col))) {
      stop("\U000274c 101: At least one column name is misspelled",
           " or does not correspond to the expected column names \n")
    }
  }
  schema <- schema[schema$names %in% exp_col]
  return(schema)
}

# Load partitioned files using Arrow
load_partition_arrow <- function(path, partition, schema = NULL) {
  filef <- id_file_format(path)
  if (filef == "parquet") {
    ds <-
      arrow::open_dataset(path, format = filef, partitioning = partition,
                          hive_style = FALSE, schema = schema)
  } else {
    ds <-
      arrow::open_dataset(path, format = filef, partitioning = partition,
                          hive_style = FALSE, col_types = schema)
  }
  df <- dplyr::collect(ds) |> factor_columns()
  return(df)
}

# Function from ?is.integer() function documentation
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  x <- as.numeric(x)
  if (length(unique(na.omit(x))) > 0) {
    abs(x - round(x)) < tol
  } else {
    return(FALSE)
  }
}

# Function to fix location column (add trailing zero)
loc_zero <- function(df) {
  if (any(nchar(df$location) == 1)) {
    df$location[which(nchar(df$location) == 1)] <-
      paste0(0, df$location[which(nchar(df$location) == 1)])
  }
  return(df)
}

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
#' @importFrom tidyr unite matches all_of
#' @importFrom dplyr mutate select .data
#'
#' @noRd
merge_sample_id <- function(df, req_colnames, merge_sample_col, js_def0,
                            add_message = NULL, partition = NULL) {
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
  sample_val <- na.omit(unlist(dplyr::distinct(df[, merge_sample_col])))
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
  df <- tidyr::unite(df, col = "type_id_sample",
                     tidyr::all_of(merge_sample_col)) |>
    dplyr::mutate(output_type_id =
                    ifelse(.data[["output_type"]] == "sample",
                           as.numeric(as.factor(.data[["type_id_sample"]])),
                           .data[["output_type_id"]])) |>
    dplyr::select(-tidyr::all_of(c("type_id_sample")))
  col_format <-
    hubValidations::create_hub_schema(js_def0, partitions = partition,
                                      r_schema = TRUE,
                                      output_type_id_datatype = "from_config")
  df$output_type_id <- eval(str2lang(paste0("as.", col_format["output_type_id"],
                                            "(df$output_type_id)")))
  if (length(unique(df[which(df$output_type == "sample"),
                       "output_type_id", TRUE])) <= 1) {
    add_message <- paste0(add_message,
                          "\n\U000274c Error 902: The submission should ",
                          "contains multiple sample output type groups, ",
                          "please verify.\n")
  }
  return(list("df" = df, "add_message" = add_message))
}



##############

# Internal function to filter data frame according to a set of task_id value
filter_df <- function(df, task_id, exclusion = NULL, required = FALSE,
                      location_fix = TRUE) {
  # transform to data.table format
  df_test <- as.data.frame(df)
  # fix location if wanted
  if (location_fix) {
    df_test <- loc_zero(df_test)
  }
  # filter
  if (is.null(exclusion)) {
    col_names <- names(task_id)
  } else {
    col_names <- grep(exclusion, names(task_id), invert = TRUE, value = TRUE)
  }
  if (required) {
    filter_var <-
      setNames(lapply(col_names,
                      function(y) unique(unlist(task_id[[y]]$required))),
               col_names)
  } else {
    filter_var <-
      setNames(lapply(col_names, function(y) unique(unlist(task_id[[y]]))),
               col_names)
  }
  filter_var <- purrr::discard(filter_var, is.null)
  for (i in seq_along(filter_var)) {
    if (grepl("date", names(filter_var)[i])) {
      df_test[[names(filter_var)[i]]] <-
        as.Date(df_test[[names(filter_var)[i]]])
      filter_var[[names(filter_var)[i]]] <-
        as.Date(filter_var[[names(filter_var)[i]]])
    }
    df_test <- df_test[which(df_test[[names(filter_var)[[i]]]] %in%
                               filter_var[[i]]), ]
  }
  text_var <- paste(names(filter_var), ": ",
                    purrr::map(filter_var, function(x) {
                      if (length(x) > 10) {
                        paste0(paste(na.omit(x[1:10]), collapse = ", "), ",
                               ...")
                      } else {
                        paste(na.omit(x[1:10]), collapse = ", ")
                      }
                    }))
  attr(df_test, "filter") <- text_var
  return(df_test)
}

# extract pairing information
paired_info <- function(df, rm_col = NULL, tasks_list = NULL,
                        verbose_col = NULL) {
  if (!is.null(rm_col)) df <- dplyr::select(df, -dplyr::contains(rm_col))
  sel_col <- grep("output|run_grou|stochas|value", names(df), value = TRUE,
                  invert = TRUE)
  df <- dplyr::arrange(df, dplyr::pick(dplyr::all_of(sel_col)))
  test_pair_list <- dplyr::distinct(df) %>%
    as.list() %>%
    purrr::map(unique)
  if (is.null(tasks_list)) { # nocov start
    paired_info <- purrr::keep(test_pair_list, function(x) length(x) > 1) %>%
      names()
  } else { # nocov end
    paired_info <- lapply(seq_along(test_pair_list), function(x) {
      if (is.null(verbose_col)) {
        if (length(test_pair_list[[x]]) > 1) {
          if (all(unlist((tasks_list[[names(test_pair_list[x])]])) %in%
                    test_pair_list[[x]])) {
            p_col <- names(test_pair_list[x])
          } else {
            t_list <- tasks_list[[names(test_pair_list[x])]]
            if ((all(t_list$required %in% test_pair_list[[x]]) &&
                   !is.null(t_list$required)) |
                  (all(t_list$optional %in% test_pair_list[[x]]) &&
                     !is.null(t_list$optional))) {
              p_col <- names(test_pair_list[x])
            } else {
              p_col <- NULL # nocov
            }
          }
        } else {
          p_col <- NULL
        }
      } else {
        if (length(test_pair_list[[x]]) > 1) {
          if (names(test_pair_list[x]) %in% verbose_col |
                !all(unlist((tasks_list[[names(test_pair_list[x])]])) %in%
                       test_pair_list[[x]])) {
            p_col <- paste0(names(test_pair_list[x]), " (",
                            paste(test_pair_list[[x]], collapse = ", "), ")")
          } else {
            p_col <- names(test_pair_list[x])
          }
        } else {
          p_col <- NULL
        }
      }
      return(p_col)
    }) %>%
      purrr::compact() %>%
      unlist() %>%
      unique()
  }
  return(paired_info)
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
