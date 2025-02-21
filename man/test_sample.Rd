% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_sample.R
\name{test_sample}
\alias{test_sample}
\title{Runs Validation Checks on the sample column}
\usage{
test_sample(
  df,
  model_task,
  pairing_col = "horizon",
  verbose = TRUE,
  verbose_col = NULL
)
}
\arguments{
\item{df}{data frame to test}

\item{model_task}{list containing round information for each id columns
and model output (type, format, etc.)}

\item{pairing_col}{column names indicating the sample pairing information. By
default: "horizon".}

\item{verbose}{Boolean, if TRUE add information about the sample pairing
information in output message}

\item{verbose_col}{character vector, name of columns to print value samples
are paired on.}
}
\description{
Validate Scenario Modeling Hub submissions: test if the  \code{sample} column
contains the expected  value.
}
\details{
This function contains 3 tests:
\itemize{
\item sample number: The submission should contain the expected number of
trajectory for each model tasks group (unique combination of \code{task_ids}
columns as specified in the \code{model_task} parameter).
\item unique sample: The submission should at least contain a unique
sample identifier by pairing group, for example if pairing_col = "horizon",
the sample identifier \code{1} should contain all the possible horizon value,
and optionally can contain the specific and multiple value for the other
task id column.
\item sample integer: The submission should contain a \code{output_type_id}
column with integer only, associated with \code{"sample"} output type.
\item required sample: If a target expected \code{"sample"} type output, the
submission should contain \code{"sample"} type output for this target.
}

Function called in the \code{validate_submission()} function, only if the
submission contains \code{"sample"} output type.
}
