# extract tasks ids column names
#' @importFrom purrr flatten map
get_tasksids_colnames <- function(js_def) {
  taskids_col <- purrr::map(js_def, "task_ids") |>
    purrr::flatten() |>
    names() |>
    unique()
  taskids_col
}

# Check location format - FIPS code of 2 character (not numeric)
location_fips_format <- function(df) {
  if (any("location" %in% names(df))) {
    if (any(nchar(df$location) == 1)) {
      vect <- unique(df$location[which(nchar(df$location) == 1)])
      warning("Some location value are missing a trailing 0. For example, ",
              paste(vect, collapse = ", "), " instead of ",
              paste(paste0(0, vect), collapse = ", "))
      df$location[which(nchar(df$location) == 1)] <-
        paste0(0, df$location[which(nchar(df$location) == 1)])
    }
    df$location <- as.character(df$location)
  }
  as.data.frame(df)
}

# File path selection
file_path_info <- function(path, hub_path, partition = NULL, round_id = NULL,
                           verbose = TRUE) {
  if (!is.null(partition)) {
    file_path <- grep(round_id, dir(paste0(hub_path, "/", path),
                                    recursive = TRUE), value = TRUE) |>
      unique()
    file_path_mess <- c(file_path[1:min(max(length(file_path) - 1, 1), 5)],
                        "etc.")
    file_path_mess <- basename(file_path_mess)

  } else {
    file_path <- basename(paste0(hub_path, "/", path))
    file_path_mess <- unique(file_path)
  }
  if (verbose) cat(paste0("Run validation on files: ",
                          paste(unique(file_path_mess), collapse = ", "), "\n"))
  file_path
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
  round_id
}

# Test of any factor columns
#' @importFrom dplyr mutate_if
factor_columns <- function(df) {
  if (any(sapply(colnames(df), function(x) is.factor(df[[x]])))) {
    warning("\U0001f7e1 Warning: At least one column is in a format: 'factor',",
            " please verify. \n The column(s) will be automatically set to ",
            " 'character'.")
  }
  df <- dplyr::mutate_if(df, is.factor, as.character)
}

# Identify file format extension - Partitioned format
#' @importFrom dplyr case_when
id_file_format <- function(path) {
  err005 <- paste0("\U000274c Error: The file format of the submission was not",
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
  filef
}

# Create schema for loading files using arrow
# optional testing of the column names matching the schema
#' @importFrom hubData create_hub_schema
#' @importFrom purrr map
#' @importFrom arrow Field int64 schema open_dataset
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
      stop("\U000274c Error: At least one column name is misspelled",
           " or does not correspond to the expected column names \n")
    }
  }
  schema <- schema[schema$names %in% exp_col]
  return(schema)
}

# Load partitioned files using Arrow
#' @importFrom arrow open_dataset
#' @importFrom dplyr collect
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
  df <- dplyr::collect(ds) |> location_fips_format() |> factor_columns()
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

#' Merge and simple tests of Sample ID columns
#'
#' If the submission files contains the multiple columns for sample ID, the
#' function will be called to: combines the columns into one `output_type_id`
#' column (on `"sample"` output type only) and tests if: the columns names are
#' correct, the two columns contains only integer values,and if the
#' concatenation does not returns an unique value.
#'
#' The function returns the output data frame (without the `merge_sample_col`)
#'
#' @importFrom tidyr unite all_of
#' @importFrom dplyr distinct filter group_by across n summarise mutate select
#' @importFrom hubValidations create_hub_schema
#' @importFrom purrr map
#'
#' @noRd
merge_sample_id <- function(df, req_colnames, merge_sample_col, js_def0, js_def,
                            checks, partition = NULL, verbose = TRUE,
                            verbose_col = NULL) {
  df_no_sample <- dplyr::filter(df, .data[["output_type"]] != "sample")
  df_sample <- dplyr::filter(df, .data[["output_type"]] == "sample")
  rm(df)
  sample_val <-
    unique(na.omit(unlist(dplyr::distinct(df_sample[, merge_sample_col]))))
  if (isFALSE(all(is_wholenumber(sample_val))) ||
        any(grepl("^0.", unique(sample_val)))) {
    message("\U000274c Error: The columns ", paste(merge_sample_col,
                                                   collapse = " and "),
            " should contain integer values only for type 'sample'.")
  }
  # Verbose
  if (verbose) {
    task_ids <- unique(unlist(purrr::map(purrr::map(js_def, "task_ids"),
                                         names)))
    n_sample <- dplyr::summarise(df_sample, n = dplyr::n(),
                                 .by = tidyr::all_of(task_ids))
    n_sample <- unique(n_sample$n)
    pair_info <- verbose_pairing(df_sample, js_def, checks, or_pair = NULL,
                                 n_sample = n_sample,
                                 verbose_col = verbose_col) |>
      purrr::map(unique)
  } else {
    pair_info <- NULL
  }

  # Merge sample ID column
  df_sample <- tidyr::unite(df_sample, col = "type_id_sample",
                            tidyr::all_of(merge_sample_col)) |>
    dplyr::mutate(output_type_id =
                    ifelse(.data[["output_type"]] == "sample",
                           as.numeric(as.factor(.data[["type_id_sample"]])),
                           .data[["output_type_id"]])) |>
    dplyr::select(-tidyr::all_of(c("type_id_sample")))
  col_format <-
    hubValidations::create_hub_schema(js_def0, partitions = NULL,
                                      r_schema = TRUE,
                                      output_type_id_datatype = "from_config")
  df_sample$output_type_id <-
    eval(str2lang(paste0("as.", col_format["output_type_id"],
                         "(df_sample$output_type_id)")))
  if (length(unique(df_sample[, "output_type_id", TRUE])) <= 1) {
    message("\n\U000274c Error: The submission should ",
            "contains multiple sample output type groups, please verify.\n")
  }
  if (nrow(df_no_sample) > 0) {
    df <- rbind(df_sample, dplyr::select(df_no_sample,
                                         -tidyr::all_of(merge_sample_col)))
  } else {
    df <- df_sample
  }
  return(list(df = df, msg = pair_info))
}

# Function for pairing information
#' @importFrom purrr map
#' @importFrom dplyr group_split
verbose_pairing <- function(df_sample, m_task, checks, or_pair, n_sample,
                            verbose_col = NULL) {
  tasks_list <- m_task[purrr::map_vec(purrr::map(purrr::map(m_task,
                                                            "output_type"),
                                                 "sample"), ~ !is.null(.x))]
  tasks_list <- tasks_list[[1]]$task_ids
  if (length(unique(df_sample$run_grouping)) > 1) {
    run_group <-
      purrr::map(dplyr::group_split(df_sample, .data[["run_grouping"]]),
                 paired_info, rm_col = c("stochastic_run", "output_type_id",
                                         "output_type",  "value"),
                 tasks_list = tasks_list, verbose_col = verbose_col) |>
      unique() |>
      purrr::map(c, or_pair)
  } else {
    run_group <- NULL
  }
  if (length(unique(df_sample$stochastic_run)) > 1) {
    sto_group <-
      purrr::map(dplyr::group_split(df_sample, .data[["stochastic_run"]]),
                 paired_info, c("run_grouping", "output_type_id",
                                "output_type", "value"),
                 tasks_list = tasks_list, verbose_col = verbose_col) |>
      unique() |>
      purrr::map(c, or_pair)
  } else {
    sto_group <- NULL
  }

  if (is.null(run_group)) {
    p_rg <- "No run grouping pairing"
  } else {
    p_rg <- paste0("Run grouping pairing: ",
                   paste(gsub("^c\\(|\\)$", "", run_group), collapse = ","))
    run_group <- purrr::map(run_group, ~ gsub(" \\(.+", "", .x))
  }
  if (is.null(sto_group)) {
    p_sg <- " No stochasticity"
  } else {
    p_sg <- paste0(" stochastic run pairing: ",
                   paste(gsub("^c\\(|\\)$", "", sto_group), collapse = ","))
    sto_group <- purrr::map(sto_group, ~ gsub(" \\(.+", "", .x))
  }

  pair_inf <- paste0(p_rg, ";", p_sg, ". Number of Samples: ",
                     paste(n_sample, collapse = ", "))
  pair_inf <- list(message = pair_inf, n_sample = n_sample,
                   run_group = run_group, sto_group = sto_group)
  return(pair_inf)
}

# extract pairing information
#' @importFrom dplyr select contains pick all_of arrange distinct
#' @importFrom purrr map keep compact
#' @importFrom data.table setDT
paired_info <- function(df, rm_col = NULL, tasks_list = NULL,
                        verbose_col = NULL) {
  if (!is.null(rm_col)) df <- dplyr::select(df, -dplyr::contains(rm_col))
  sel_col <- grep("output|run_grou|stochas|value", names(df), value = TRUE,
                  invert = TRUE)
  df <- dplyr::arrange(df, dplyr::pick(dplyr::all_of(sel_col)))
  test_pair_list <- data.table::setDT(df) |>
    as.list() |>
    purrr::map(unique)
  if (is.null(tasks_list)) {
    paired_info <- purrr::keep(test_pair_list, function(x) length(x) > 1) |>
      names()
  } else {
    paired_info <- lapply(seq_along(test_pair_list), function(x) {
      if (is.null(verbose_col)) {
        if (length(test_pair_list[[x]]) > 1) {
          if (sum(unlist((tasks_list[[names(test_pair_list[x])]])) %in%
                    test_pair_list[[x]]) > 1 |
                (all(unlist((tasks_list[[names(test_pair_list[x])]])) %in%
                       test_pair_list[[x]]))) {
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
      p_col
    }) |>
      purrr::compact() |>
      unlist() |>
      unique()
  }
  return(paired_info)
}


##############
# nocov start
# Internal function to filter data frame according to a set of task_id value
#' @importFrom purrr discard map
#' @importFrom stats setNames
filter_df <- function(df, task_id, exclusion = NULL, required = FALSE,
                      location_fix = TRUE) {
  # transform to data.table format
  df_test <- as.data.frame(df)
  # fix location if wanted
  if (location_fix) {
    df_test <- location_fips_format(df_test)
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
    if (grepl("date", names(filter_var)[i], fixed = TRUE)) {
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
# nocov end
