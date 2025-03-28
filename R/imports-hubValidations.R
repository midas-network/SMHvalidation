# Import from hubValidations with slight modification for performance

#' @importFrom purrr map compact map2 imap_chr imap
#' @importFrom cli format_inline qty
summarise_invalid_values <- function(valid_tbl, config_tasks, round_id) {
  cols <- setdiff(names(valid_tbl), c("value", "valid", "rowid"))
  uniq_tbl <- purrr::map(valid_tbl[cols], unique)
  uniq_config <- hubValidations:::get_round_config_values(config_tasks,
                                                          round_id, NULL)[cols]
  invalid_vals <- purrr::map2(uniq_tbl, uniq_config, ~.x[!.x %in% .y]) |>
    purrr::compact()
  if (length(invalid_vals) != 0L) {
    invalid_vals <- unique(invalid_vals[1:5])
    invalid_vals_msg <-
      purrr::imap_chr(invalid_vals,
                      ~cli::format_inline("For example: ",
                                          "Column {.var {.y}} contains invalid",
                                          " {cli::qty(length(.x))}\n        ",
                                          "value{?s} {.val {.x}}.")) |>
      paste(collapse = " ")
  } else {
    invalid_vals_msg <- NULL
  }
  invalid_val_idx <- purrr::imap(invalid_vals, ~which(valid_tbl[[.y]] %in%
                                                        .x)) |>
    unlist(use.names = FALSE) |>
    unique()
  invalid_row_idx <- which(is.na(valid_tbl$valid))
  invalid_combs_idx <- setdiff(invalid_row_idx, invalid_val_idx)
  if (length(invalid_combs_idx) == 0L) {
    invalid_combs_msg <- NULL
  } else {
    invalid_combs_idx <- unique(valid_tbl$rowid[invalid_combs_idx][1:5])
    invalid_combs_msg <-
      cli::format_inline("Additionally {cli::qty(length(invalid_combs_idx))} ",
                         "row{?s}\n      {.val {invalid_combs_idx}} ",
                         "{cli::qty(length(invalid_combs_idx))}\n      ",
                         "{?contains/contain} invalid combinations of valid ",
                         "values.\n      See {.var error_tbl} for examples.")
  }
  list(msg = paste(invalid_vals_msg, invalid_combs_msg, sep = "\n"),
       invalid_combs_idx = invalid_combs_idx)
}


#' @importFrom hubUtils read_config
#' @importFrom tibble rowid_to_column
#' @importFrom purrr imap list_rbind
#' @importFrom hubValidations capture_check_cnd
check_tbl_values <- function(tbl, round_id, file_path, hub_path) {
  config_tasks <- hubUtils::read_config(hub_path, "tasks")
  valid_tbl <- tibble::rowid_to_column(tbl) |>
    split(f = tbl$output_type) |>
    purrr::imap(
      ~hubValidations:::check_values_by_output_type(tbl = .x, output_type = .y,
                                                    config_tasks = config_tasks,
                                                    round_id = round_id,
                                                    derived_task_ids = NULL)) |>
    purrr::list_rbind()
  check <- !any(is.na(valid_tbl$valid))
  if (check) {
    details <- error_tbl <- NULL
  } else {
    error_summary <- summarise_invalid_values(valid_tbl, config_tasks, round_id)
    details <- error_summary$msg
    if (length(error_summary$invalid_combs_idx) == 0L) {
      error_tbl <- NULL
    } else {
      error_tbl <- tbl[error_summary$invalid_combs_idx, names(tbl) != "value"]
    }
  }
  capture_check_cnd(check = check, file_path = file_path,
                    msg_subject = "{.var tbl}", msg_attribute = "",
                    msg_verbs = c("contains valid values/value combinations.",
                                  paste0("contains invalid values/value ",
                                         "combinations.")),
                    error_tbl = error_tbl, error = TRUE, details = details)
}

#' @importFrom hubValidations capture_check_cnd
#' @importFrom dplyr summarize n
#' @importFrom data.table `.N` data.table
check_tbl_rows_unique <- function(tbl, file_path, hub_path) {
  tbl[["values"]] <- NULL
  tbl <- data.table::data.table(tbl)
  tbl <- tbl[, .N, by = names(tbl)]
  tbl <- dplyr::filter(tbl, .data[["N"]] > 1)
  check <- nrow(tbl) == 0
  if (check) {
    details <- NULL
  } else {
    details <- cli::format_inline("Rows containing duplicate combinations.")
  }
  capture_check_cnd(check = check, file_path = file_path,
                    msg_subject =
                      cli::format_inline("All combinations of task ID column/",
                                         "{.var output_type}/",
                                         "{.var output_type_id} values"),
                    msg_attribute = "unique.", msg_verbs = c("are", "must be"),
                    details = details)
}
