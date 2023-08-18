#' Read CSV, ZIP, GZ pr PQT files
#'
#' Reads a file and output a data frame. The function can read files in CSV, ZIP,
#' GZ and PQT format
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
    file_name <- unzip(path,list = TRUE)[,"Name", TRUE]
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

# Function from ?is.integer() function documentation
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  x <- as.numeric(x)
  abs(x - round(x)) < tol
}

# Internal function to filter data frame according to a set of task_id value
filter_df <- function(df, task_id, exclusion = NULL, required = FALSE,
                      location_fix = TRUE) {
  # transform to data.table format
  df_test <- as.data.frame(df)
  # fix location if wanted
  if (location_fix) {
    if (any(nchar(df_test$location) == 1)) {
      df_test$location[which(nchar(df_test$location) == 1)] <- paste0(
        0, df_test$location[which(nchar(df_test$location) == 1)])
    }
  }
  # filter
  if (is.null(exclusion)) {
    col_names <- names(task_id)
  } else {
    col_names <- grep(exclusion, names(task_id), invert = TRUE, value = TRUE)
  }
  if (required) {
    filter_var <- setNames(lapply(col_names, function(y)
      unique(unlist(task_id[[y]]$required))), col_names)
  } else {
    filter_var <- setNames(lapply(col_names, function(y)
      unique(unlist(task_id[[y]]))), col_names)
  }
  filter_var <- purrr::discard(filter_var, is.null)
  for (i in 1:length(filter_var)) {
    if (grepl("date", names(filter_var)[i])) {
      df_test[[names(filter_var)[i]]] <- as.Date(
        df_test[[names(filter_var)[i]]])
      filter_var[[names(filter_var)[i]]] <- as.Date(
        filter_var[[names(filter_var)[i]]])
    }
    df_test <- df_test[which(
      df_test[[names(filter_var)[[i]]]] %in% filter_var[[i]]), ]
  }
  text_var <- paste(
    names(filter_var), ": ",  purrr::map(
      filter_var,  function(x) {
        if (length(x) > 10) {
          paste0(paste(na.omit(x[1:10]), collapse = ", "), ", ...")
        } else {
          paste(na.omit(x[1:10]), collapse = ", ")
        }
      }))

  attr(df_test, "filter") <- text_var
  return(df_test)
}
