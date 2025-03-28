# nocov start
#' Runs Validation Checks on the required value
#'
#' **DEPRECATED** <br><br>
#' Validate Scenario Modeling Hub submissions: test if all the required value
#' are present in the submission file
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#' `model_task` should match a specific round model tasks from the
#' `tasks.json` associated with the hub. The json is expected to follow
#' the [hubverse](https://hubverse.io/en/latest/user-guide/hub-config.html)
#' schema at least version 5.0
#'
#' As the function was deprecated, it will not be updated anymore. It was
#' updated a last time to match the 5.0 hubverse schema version.
#'
#'@importFrom purrr compact map
#'@importFrom dplyr mutate_if mutate bind_rows distinct select all_of setdiff
#'@importFrom tidyr drop_na
#'@importFrom stats setNames
#'@export
test_req_value <- function(df, model_task) {

  warning("Function deprecated")

  req_df <- lapply(model_task, function(x) {
    req_df <- setNames(lapply(names(x$task_ids),
                              function(z) {
                                unlist(x$task_id[[z]]$required) |>
                                  unique()
                              }), names(x$task_ids))
    req_df <- purrr::compact(req_df) |>
      expand.grid() |>
      dplyr::mutate_if(is.factor, as.character) |>
      dplyr::mutate(origin_date = as.Date(.data[["origin_date"]]))
    if (!"horizon" %in% colnames(req_df)) req_df$horizon <- NA
    req_df$horizon <- suppressWarnings(as.integer(req_df$horizon))
    req_df
  }) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    tidyr::drop_na()

  col_sel <- names(req_df)
  test_df <- dplyr::select(df, dplyr::all_of(col_sel)) |>
    dplyr::distinct() |>
    dplyr::mutate(origin_date = as.Date(.data[["origin_date"]])) |>
    loc_zero()

  res <- dplyr::setdiff(req_df, test_df)
  if (nrow(res) > 0) {
    err <- purrr::map(as.list(res), unique)
    err <- paste(names(err), purrr::map(err, as.character), sep = ": ",
                 collapse = "\n")
    message("\U000274c Error: The submission is missing some ",
            "required values, please check: \n ", err)
  }
  invisible(NULL)
}
# nocov end
