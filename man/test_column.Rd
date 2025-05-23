% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_column.R
\name{test_column}
\alias{test_column}
\title{Runs Validation Checks on Columns names and number}
\usage{
test_column(df, req_colnames, file_name = "")
}
\arguments{
\item{df}{data frame to test}

\item{req_colnames}{vector of required column names}

\item{file_name}{optional, name of the file tested}
}
\description{
\strong{DEPRECATED} \if{html}{\out{<br>}}\if{html}{\out{<br>}}
Validate Scenario Modeling Hub submissions: names and number of columns.
}
\details{
This function tests:
*Name: The names of the columns are corresponding to the expected
column names. If one column is misspelled or is missing, the validation
will stop here with an error message and no other tests will be perform on
the submission.
*Number: The submission should contains the expected number of
columns.

The function was deprecated, it will not be updated anymore. It was
updated a last time to match the 5.0 hubverse schema version.
}
