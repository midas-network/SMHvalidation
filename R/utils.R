#' Read CSV, ZIP, GZ pr PQT files
#'
#' Reads a file and output a data frame. The function can read files in CSV,
#'  ZIP, GZ and PQT format
#'
#' @param path path of the files which the date are to be read from
#'
#' @importFrom utils read.csv unzip
#' @importFrom arrow read_parquet
#'
#' @examples
#' \dontrun{
#'  df <- read_files("PATH/TO/FILE")
#' }
#'
#' @export
read_files <- function(path) {
  if (grepl(".csv$", basename(path))) {
    df <- read.csv(path, sep = ",", na.strings = c("", "NA", "NaN"))
  }
  if (grepl(".zip$", basename(path))) {
    file_name <- unzip(path, list = TRUE)[, "Name", TRUE]
    unzip(path)
    df <- read.csv(file_name[1], sep = ",", na.strings = c("", "NA", "NaN"))
    file.remove(file_name)
  }
  if (grepl(".gz$", basename(path))) {
    file_name <- gzfile(path)
    df <- read.csv(file_name, sep = ",", na.strings = c("", "NA", "NaN"))
  }
  if (grepl(".pqt$|.parquet$", basename(path))) {
    df <- arrow::read_parquet(path, as_data_frame = TRUE)
  }
  if (any("location" %in% names(df))) df$location <- as.character(df$location)
  return(df)
}


load_partition_arrow <- function(path, js_def, js_def_round, partition,
                                 round_id, merge_sample_col = FALSE) {
  if (all(grepl("parquet$|.pqt$", dir(path, recursive = TRUE)))) {
    filef <- "parquet"
  } else if (all(grepl(".csv$", dir(path, recursive = TRUE)))) {
    filef <- "csv"
  } else {
    err005 <-
      paste0("\U000274c Error 005: The file format of the submission was not",
             " recognized, please use one unique file or multiple parquet ",
             "files. For more information, please look at the documentation ",
             "of the hub. \n")
    cat(err005)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }
  exp_col <- c(unique(names(unlist(purrr::map(js_def_round$model_tasks,
                                              "task_ids"), FALSE))),
               "output_type", "output_type_id", "value")
  schema <- hubData::create_hub_schema(js_def)
  if (merge_sample_col) {
    exp_col <- c(exp_col, "run_grouping", "stochastic_run")
    schema <- c(schema$fields,
                arrow::Field$create("run_grouping", arrow::int64()),
                arrow::Field$create("stochastic_run", arrow::int64()))
    schema <- arrow::schema(schema)
  }
  files_path <- grep(round_id,
                     dir(path, full.names = TRUE,
                         recursive = TRUE), value = TRUE)
  col_names <- arrow::open_dataset(sources = files_path,
                                   format = filef)$schema$names
  col_names <- unique(c(col_names, partition))
  if (!(all(exp_col %in% col_names)) || !(all(col_names %in% exp_col))) {
    colnames_test <- paste0("\U000274c Error 101: At least one column name is ",
                            "misspelled or does not correspond to the expected",
                            " column names \n")
    cat(colnames_test)
    stop(" The submission contains an issue, the validation was not run, ",
         "please see information above.")
  }
  schema <- schema[schema$names %in% exp_col]
  if (filef == "parquet") {
    ds <-
      arrow::open_dataset(path, format = filef, partitioning = partition,
                          hive_style = FALSE, schema = schema)
  } else {
    ds <-
      arrow::open_dataset(path, format = filef, partitioning = partition,
                          hive_style = FALSE, col_types = schema)
  }

  df <- dplyr::collect(ds) %>%
    dplyr::mutate_if(is.factor, as.character)

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
  test_pair_list <- dplyr::distinct(df) %>%
    as.list() %>%
    purrr::map(unique)
  if (is.null(tasks_list)) {
    paired_info <- purrr::keep(test_pair_list, function(x) length(x) > 1) %>%
      names()
  } else {
    paired_info <- lapply(seq_along(test_pair_list), function(x) {
      if (is.null(verbose_col)) {
        if (length(test_pair_list[[x]]) > 1) {
          if (all(unlist((tasks_list[[names(test_pair_list[x])]])) %in%
                  test_pair_list[[x]])) {
            p_col <- names(test_pair_list[x])
          } else {
            if (all(tasks_list[[names(test_pair_list[x])]]$required %in%
                    test_pair_list[[x]]) |
                all(tasks_list[[names(test_pair_list[x])]]$optional %in%
                    test_pair_list[[x]])) {
              p_col <- paste0(names(test_pair_list[x]), " (",
                              paste(test_pair_list[[x]], collapse = ", "), ")")
            } else {
              p_col <- NULL
            }
          }
        } else {
          p_col <- NULL
        }
      } else {
        if (length(test_pair_list[[x]]) > 1) {
          if (verbose_col == names(test_pair_list[x])) {
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
      unlist()
  }
  return(paired_info)
}
