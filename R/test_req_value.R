#' Runs Validation Checks on the required value
#'
#' Validate Scenario Modeling Hub submissions: test if all the required value
#' are present in the submission file
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#' Function called in the `validate_submission()` function, only if the
#' submission contains `"sample"` output type.
#'
#'@importFrom purrr compact map
#'@importFrom dplyr mutate_if mutate bind_rows distinct select all_of setdiff
#'@importFrom tidyr drop_na
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
    return(req_df)
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
