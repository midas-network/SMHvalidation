% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_validation_plots.R
\name{generate_validation_plots}
\alias{generate_validation_plots}
\title{Generate Validation Plots}
\usage{
generate_validation_plots(
  path_proj,
  target_data = NULL,
  save_path = dirname(path_proj),
  y_sqrt = FALSE,
  plot_quantiles = c(0.025, 0.975),
  partition = NULL
)
}
\arguments{
\item{path_proj}{path to submission file(s) in the expected Scenario
. Modeling Hub standard format}

\item{target_data}{data frame containing the
observed data in the Hubverse time series target data standard format.
Please find additional information on the
\href{https://hubverse.io/en/latest/user-guide/target-data.html}{hubverse}
website. Set to \code{NULL} (default), to NOT include comparison with observed
data.}

\item{save_path}{character vector, path to the saving folder for the PDF
output. By default, the direction of \code{path_proj} will be used}

\item{y_sqrt}{boolean, by default FALSE}

\item{plot_quantiles}{numeric vector, quantiles to use for plotting (should
correspond to the quantiles from the \code{proj_data} parameter). By default,
\code{0.025} and \code{0.975}}

\item{partition}{vector, for csv and parquet files, allow to validate files
in a partition format, see \code{arrow} package for more information, and
\code{arrow::write_dataset()}, \code{arrow::open_dataset()} functions. By default,
\code{NULL}, imput submission file is not partitionned.}
}
\description{
Generate a PDF files containing quantiles information about the submission
file: plot by states and possibility to add comparison to target data.
}
