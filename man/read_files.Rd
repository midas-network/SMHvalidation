% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{read_files}
\alias{read_files}
\title{Read CSV, ZIP, GZ pr PQT files}
\usage{
read_files(path)
}
\arguments{
\item{path}{path of the files which the date are to be read from}
}
\description{
Reads a file and output a data frame. The function can read files in CSV, ZIP,
GZ and PQT format
}
\examples{
\dontrun{
 df <- read_files("PATH/TO/FILE")
}

}
