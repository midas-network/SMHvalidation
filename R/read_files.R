#' Read CSV, ZIP, GZ pr PQT files
#'
#' Reads a file and output a data frame. The function can read files in CSV,
#'  ZIP, GZ and PQT/parquet format (not partitioned)
#'
#' @param path path of the file
#' @param na_string character vector to be interpreted as `NA` values
#' @param sep character, separator character between columns
#'
#' @importFrom utils read.csv unzip
#' @importFrom arrow read_parquet
#'
#' @examples
#' \dontrun{
#'  df <- read_files("PATH/TO/FILE")
#' }
#'
#' @details
#' Returns a Warning if the file contains a `location` column with the US FIPS
#' code of the location missing the leading 0, for example `"6"` instead of
#' `"06"` for California, US. By default, the missing leading `0` will be
#' added.
#'
#' Returns a Warning if a column is in a factor format. By default, the column
#' will be forced to a character format
#'
#' @importFrom arrow read_parquet
#'
#' @export
read_files <- function(path, na_string = c("", "NA", "NaN"), sep = ",") {
  if (grepl(".csv$", basename(path)))
    df <- read.csv(path, sep = sep, na.strings = na_string)
  if (grepl(".zip$", basename(path))) {
    file_name <- unzip(path, list = TRUE)[, "Name", TRUE]
    unzip(path)
    df <- read.csv(file_name[1], sep = sep, na.strings = na_string)
    file.remove(file_name)
  }
  if (grepl(".gz$", basename(path))) {
    file_name <- gzfile(path)
    df <- read.csv(file_name, sep = sep, na.strings = na_string)
  }
  if (grepl(".pqt$|.parquet$", basename(path))) {
    df <- arrow::read_parquet(path, as_data_frame = TRUE)
  }
  df <- location_fips_format(df)
  df <- factor_columns(df)
  return(df)
}
