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
                        merge_sample_col = NULL, r_schema = NULL) {
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
