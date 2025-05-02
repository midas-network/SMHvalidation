#' Generate Validation Plots
#'
#' Generate a PDF files containing quantiles information about the submission
#' file: plot by states and possibility to add comparison to target data.
#'
#' @param path_proj path to submission file(s) in the expected Scenario
#'. Modeling Hub standard format
#' @param target_data data frame containing the
#' observed data in the Hubverse time series target data standard format.
#' Please find additional information on the
#' [hubverse](https://hubverse.io/en/latest/user-guide/target-data.html)
#' website. Set to `NULL` (default), to NOT include comparison with observed
#' data.
#' @param save_path character vector, path to the saving folder for the PDF
#' output. By default, the direction of `path_proj` will be used
#' @param y_sqrt boolean, by default FALSE
#' @param plot_quantiles numeric vector, quantiles to use for plotting (should
#' correspond to the quantiles from the `proj_data` parameter). By default,
#' `0.025` and `0.975`
#' @param partition vector, for csv and parquet files, allow to validate files
#' in a partition format, see `arrow` package for more information, and
#' `arrow::write_dataset()`, `arrow::open_dataset()` functions. By default,
#' `NULL`, imput submission file is not partitionned.
#'
#' @importFrom tidyr separate matches
#' @importFrom dplyr mutate collect .data
#' @importFrom arrow open_dataset
#'
#' @export
generate_validation_plots <- function(path_proj, target_data = NULL,
                                      save_path = dirname(path_proj),
                                      y_sqrt = FALSE,
                                      plot_quantiles = c(0.025, 0.975),
                                      partition = NULL) {

  # SETUP
  date_pttrn <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
  file_extension <- ".csv|.zip|.gz|.pq|.parquet"
  if (is.null(partition)) {
    file_ <- basename(path_proj)
    team_model_name <- gsub(paste0(date_pttrn, "(_|-)|", file_extension),
                            "",  file_)
  } else {
    file_ <- dir(path_proj, recursive = TRUE)
    team_model_name <- gsub(paste0("(.+?)?", date_pttrn, "(\\/.*|-)|",
                                   file_extension),  "", basename(file_))
    team_model_name <- unique(team_model_name)
  }

  projection_date <- gsub(paste0("(?<=", date_pttrn, ").+"), "", file_,
                          perl = TRUE)
  projection_date <- unique(as.Date(projection_date))
  save_path <- file.path(unique(save_path),
                         paste0(projection_date, "_", unique(team_model_name),
                                "_plots.pdf"))

  # Ground Truth Data
  if (!is.null(target_data)) {
    target_data <- tidyr::separate(target_data, tidyr::matches("target"),
                                   into = c("type", "outcome"), sep = " ") |>
      dplyr::mutate(pre_gs_end = .data[["date"]] < projection_date)
    target_data <- add_filter_col(target_data)
  }

  # Projections - Read files
  if (is.null(partition)) {
    proj_data <- suppressMessages(read_files(path_proj))
  } else {
    proj_data <- load_partition_arrow(path_proj, partition = partition)
  }

  # Projection - Standardized format
  proj_data <-
    add_filter_col(proj_data) |>
    dplyr::filter(grepl(paste0("inc case|inc death|inc hosp|inc inf",
                               "cum case|cum death|cum hosp|inc inf"),
                        .data[["target"]]) &
                    grepl("quantile", .data[["output_type"]], fixed = TRUE)) |>
    dplyr::mutate(date = as.Date(.data[["origin_date"]]) - 1 +
                    (.data[["horizon"]] * 7),
                  origin_date = as.Date(.data[["origin_date"]]))

  # Create PDF of State Plots
  if (nrow(proj_data) > 0) {
    make_state_plot_pdf(proj_data = proj_data, target_data = target_data,
                        team_model_name = team_model_name,
                        projection_date = unique(proj_data$origin_date),
                        save_path = save_path, plot_quantiles = plot_quantiles,
                        y_sqrt = y_sqrt)
  } else {
    message("No output type quantiles for incident/cumulative case, death",
            "hospitalization or infection found in the file - no plot",
            "generated.")
    return(NULL)
  }

}
