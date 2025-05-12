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
    invalid_vals <-
      purrr::map(invalid_vals,
                 ~ if (length(.x) > 25) unique(.x)[1:25] else unique(.x))
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
    invalid_combs_msg <- NULL # nocovr
  } else {
    invalid_combs_idx <- unique(valid_tbl$rowid[invalid_combs_idx][1:5])
    invalid_combs_msg <-
      cli::format_inline("Additionally {cli::qty(length(invalid_combs_idx))} ",
                         "row{?s}\n      {.val {invalid_combs_idx}} ",
                         "{cli::qty(length(invalid_combs_idx))}\n      ",
                         "{?contains/contain} invalid combinations of valid ",
                         "values.\n ")
  }
  list(msg = paste(invalid_vals_msg, invalid_combs_msg, sep = "\n"),
       invalid_combs_idx = invalid_combs_idx)
}

#' @importFrom hubValidations expand_model_out_grid
#' @importFrom hubUtils is_v3_config
#' @importFrom dplyr left_join
check_values_by_output_type <- function(tbl, output_type, config_tasks,
                                        round_id) {

  accepted_vals <-
    hubValidations::expand_model_out_grid(config_tasks = config_tasks,
                                          round_id = round_id,
                                          all_character = TRUE,
                                          output_types = output_type,
                                          derived_task_ids = NULL)
  accepted_vals$valid <- TRUE
  if (hubUtils::is_v3_config(config_tasks) && output_type ==
        "sample") {
    tbl[tbl$output_type == "sample", "output_type_id"] <- NA
  }
  dplyr::left_join(tbl, accepted_vals, by = setdiff(names(tbl),
                                                    c("value", "rowid")))
}

#' @importFrom hubUtils read_config
#' @importFrom tibble rowid_to_column
#' @importFrom purrr imap list_rbind
#' @importFrom hubValidations capture_check_cnd
check_tbl_values <- function(tbl, round_id, file_path, hub_path) {
  config_tasks <- hubUtils::read_config(hub_path, "tasks")
  valid_tbl <- split(data.table::setDT(tbl), f = tbl$output_type) |>
    purrr::imap(\(.x, .y) {
      check_values_by_output_type(tbl = data.frame(.x), output_type = .y,
                                  config_tasks = config_tasks,
                                  round_id = round_id)
    }) |>
    purrr::list_rbind()
  check <- !any(is.na(valid_tbl$valid))
  if (check) {
    details <- NULL
  } else {
    valid_tbl <- tibble::rowid_to_column(valid_tbl)
    error_summary <- summarise_invalid_values(valid_tbl, config_tasks, round_id)
    details <- error_summary$msg
  }
  capture_check_cnd(check = check, file_path = file_path,
                    msg_subject = "{.var tbl}", msg_attribute = "",
                    msg_verbs = c("contains valid values/value combinations.",
                                  paste0("contains invalid values/value ",
                                         "combinations.")), error = TRUE,
                    details = details)
}

#' @importFrom hubValidations capture_check_cnd
#' @importFrom dplyr summarize n
#' @importFrom data.table `.N` data.table
check_tbl_rows_unique <- function(tbl, file_path, hub_path) {
  tbl[["values"]] <- NULL
  tbl <- data.table::setDT(tbl)[, .N, by = names(tbl)]
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

compare_values_to_config <- function(tbl, output_type, output_type_config) {
  if (any(is.null(tbl), is.null(output_type_config))) {
    return(NULL)
  }
  details <- NULL
  values0 <- unique(tbl$value)
  rm(tbl)
  config <- output_type_config[[output_type]][["value"]]
  values_type <- config$type
  values <- hubValidations:::coerce_values(values0, values_type)
  if (any(is.na(values))) {
    invalid_vals <- values0[is.na(values)]
    details <-
      c(details,
        cli::format_inline("{cli::qty(length(invalid_vals))} Value{?s} ",
                           "{.val {invalid_vals}} cannot be coerced ",
                           "to expected data type {.val {values_type}} ",
                           "for output type {.val {output_type}}."))
    if (length(invalid_vals) > 0) values <- stats::na.omit(values)
    if (length(values) == 0L) return(details)
  }
  invalid_int <-
    hubValidations:::detect_invalid_int(original_values = values0,
                                        coerced_values = values)
  if (invalid_int$check) {
    details <-
      c(details,
        cli::format_inline("{cli::qty(length(invalid_int$vals))} Value{?s} ",
                           "{.val {invalid_int$vals}}cannot be ",
                           "coerced to expected data type ",
                           "{.val {values_type}} for output type ",
                           "{.val {output_type}}."))
  }
  if (any(names(config) == "maximum")) {
    value_max <- config[["maximum"]]
    is_invalid <- values > value_max
    if (any(is_invalid)) {
      details <-
        c(details,
          cli::format_inline("{cli::qty(sum(is_invalid))} Value{?s} ",
                             "{.val {unique(values[is_invalid])}} ",
                             "{cli::qty(sum(is_invalid))}{?is/are} ",
                             "greater than allowed maximum value ",
                             "{.val {value_max}} for output type ",
                             "{.val {output_type}}."))
    }
  }
  if (any(names(config) == "minimum")) {
    value_min <- config[["minimum"]]
    is_invalid <- values < value_min
    if (any(is_invalid)) {
      details <-
        c(details,
          cli::format_inline("{cli::qty(sum(is_invalid))} Value{?s} ",
                             "{.val {unique(values[is_invalid])}} ",
                             "{cli::qty(sum(is_invalid))}{?is/are} ",
                             "smaller than allowed minimum value ",
                             "{.val {value_min}} for output type ",
                             "{.val {output_type}}."))
    }
  }
  details
}

check_value_col_by_output_type <- function(tbl, output_type, config_tasks,
                                           round_id) {
  purrr::map2(.x =
                hubValidations:::match_tbl_to_model_task(tbl, config_tasks,
                                                         round_id,
                                                         output_type,
                                                         derived_task_ids =
                                                         NULL),
              .y = hubValidations:::get_round_output_types(config_tasks,
                                                           round_id),
              function(.x, .y) {
                compare_values_to_config(tbl = .x, output_type_config = .y,
                                         output_type = output_type)
              }) |> unlist(use.names = TRUE)
}


#' @importFrom hubValidations capture_check_cnd
#' @importFrom dplyr across
#' @importFrom purrr imap
check_tbl_value_col <- function(tbl, round_id, file_path, hub_path) {
  config_tasks <- read_config(hub_path, "tasks")
  tbl <- dplyr::mutate(tbl,
                       dplyr::across(-dplyr::contains("value"), as.character))
  details <- split(tbl, f = tbl$output_type) |>
    purrr::imap(
      \(.x, .y) {
        check_value_col_by_output_type(tbl = .x, output_type = .y,
                                       config_tasks = config_tasks,
                                       round_id = round_id)
      }
    ) |>
    unlist(use.names = TRUE)

  check <- is.null(details)

  capture_check_cnd(
    check = check,
    file_path = file_path,
    msg_subject = "Values in column {.var value}",
    msg_verbs = c("all", "are not all"),
    msg_attribute = "valid with respect to modeling task config.",
    details = details,
    error = TRUE
  )
}

#' @importFrom rlang arg_match inherits_any
#' @importFrom purrr map_lgl
is_check_class <- function(x,
                           class = c(
                             "check_success", "check_failure",
                             "check_exec_warn", "check_error",
                             "check_exec_error", "check_info"
                           )) {
  class <- rlang::arg_match(class)
  purrr::map_lgl(x, ~ rlang::inherits_any(.x, class))
}


#' Store validation output in a simple object
#'
#' @param msg validation output
#' @param rm_valid_check Boolean, remove valid check and `spl_non_compound`
#' output, by default FALSE.
#'
#' @importFrom dplyr case_when
#' @importFrom purrr map_chr
#' @export
store_msg_val <- function(msg, rm_valid_check = FALSE) {
  txt <- paste(
    dplyr::case_when(is_check_class(msg, "check_success") ~ "\U002705",
                     is_check_class(msg, "check_failure") ~ "\U002757",
                     is_check_class(msg, "check_exec_warn") ~ "\U000021",
                     is_check_class(msg, "check_error") ~ "\U001F6AB",
                     is_check_class(msg, "check_exec_error") ~ "\U002588",
                     is_check_class(msg, "check_info") ~ "\U002139",
                     TRUE ~ "*"),
    paste0("[", names(msg), "]"),
    purrr::map_chr(msg, "message"),
    sep = ": "
  )
  if (rm_valid_check)
    txt <- purrr::discard(txt, ~grepl("\U002705|spl_non_compound", .x))
  if (length(txt) > 0) {
    txt <- paste(txt, collapse = "\n")
  } else {
    txt <- "No issue reported"
  }
  txt
}
