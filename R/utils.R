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

