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
    location_fips_format()

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

extract_output_type <- function(x) {
  outtype_df <- list()
  for (i in names(x$output_type)) {
    if (i == "sample") {
      output_type_id <- "..."
    } else {
      output_type_id <-
        x$output_type[[i]]$output_type_id$required
    }
    outtype_df[[i]] <- output_type_id
  }
  outtype_df
}


create_table <- function(list_targ, outtype_df) {
  if (length(purrr::compact(list_targ[!names(list_targ) %in%
                                      names(outtype_df)])) == 0) {
    req_df <- data.frame()
  } else {
    req_df <- purrr::compact(list_targ) |>
      expand.grid() |>
      tidyr::pivot_longer(tidyr::all_of(names(outtype_df)),
                          names_to = "output_type",
                          values_to = "output_type_id")
    if (!"horizon" %in% colnames(req_df)) req_df$horizon <- NA
    if (!"target" %in% colnames(req_df)) req_df$target <- NA
    req_df <- dplyr::mutate_all(req_df, as.character)
    req_df
  }
}

#' @importFrom hubValidations capture_check_cnd
#' @importFrom tidyr all_of pivot_longer drop_na
#' @importFrom dplyr mutate_all mutate bind_rows distinct setdiff inner_join
#' @importFrom purrr compact map
check_df_values_required <- function(test_df, model_task, file_path) {

  test_df <- dplyr::select(test_df, -tidyr::all_of(c("value"))) |>
    dplyr::distinct() |>
    dplyr::mutate(output_type_id = ifelse(.data[["output_type"]] == "sample",
                                          "...", .data[["output_type_id"]])) |>
    dplyr::distinct() |>
    dplyr::mutate_all(as.character) |>
    location_fips_format()

  err <- purrr::map(model_task, function(x) {
    opt_err <- err <- NULL
    if (is.null(x$task_ids$target$required)) {
      opt_targ <- purrr::map(x$task_ids, unlist)
      req_targ <- NULL
    } else {
      req_targ <- purrr::map(x$task_ids, "required")
      opt_targ <- purrr::map(x$task_ids, "optional")
    }
    outtype_df <- extract_output_type(x)

    if (!is.null(req_targ)) {
      req_df <- create_table(c(req_targ, outtype_df), outtype_df)
      test <- dplyr::select(test_df, tidyr::all_of(colnames(req_df))) |>
        dplyr::distinct()
      df_res <- dplyr::setdiff(req_df, test)
      if (nrow(df_res) > 0) {
        err <- purrr::map(as.list(df_res), unique)
        err <- paste(names(err), purrr::map(err, as.character), sep = ": ",
                     collapse = "\n")
      }
    }

    test <- dplyr::select(test_df,
                          tidyr::all_of(c(names(opt_targ), "output_type",
                                          "output_type_id"))) |>
      dplyr::filter(.data[["target"]] %in% c(opt_targ$target),
                    .data[["output_type"]] %in% names(outtype_df),
                    .data[["output_type_id"]] %in% unlist(outtype_df)) |>
      distinct()
    opt_targ <-
      lapply(seq_along(opt_targ),
             function(y) {
               vect <- opt_targ[[y]][opt_targ[[y]] %in%
                                       unique(test[,names(opt_targ[y])])]
               if (length(vect) == 0) vect <- NULL
               vect
               }) |>
      setNames(names(opt_targ))
    opt_df <- create_table(c(opt_targ, outtype_df), outtype_df)
    if (nrow(opt_df) > 0) {
      df_res_opt <-
        dplyr::setdiff(opt_df,
                       dplyr::select(test, tidyr::all_of(c(names(opt_df),
                                                           "output_type",
                                                           "output_type_id"))))
      if (any(grepl("cdf|quantile|pmf", df_res_opt$output_type)) &
          nrow(df_res_opt) > 0) {
        opt_err <- purrr::map(as.list(df_res_opt), unique)
        opt_err <- paste(names(opt_err), purrr::map(opt_err, as.character),
                         sep = ": ", collapse = ";\n")
      }
    }

    if (is.null(err) & is.null(opt_err)) {
      NULL
    } else {
      list("req" = err, "opt" = opt_err)
    }
  })

  req_err <- paste(unlist(purrr::map(purrr::map(err, "req"), unique)),
                   collapse = "\n\n")
  opt_err <- paste(unlist(purrr::map(purrr::map(err, "opt"), unique)),
                   collapse = "\n\n")
  err <- paste(trimws(unique(c(req_err, opt_err))), collapse = "\n ")
  if (nchar(req_err) > 0) {
    capture_check_cnd(
      check =  nchar(err) == 0,
      file_path = file_path,
      error = TRUE,
      msg_subject = "Required task ID/output type/output_type_id combinations",
      msg_attribute = NULL,
      msg_verbs = c("all present.", "missing."),
      details = paste("Please verify: \n\n", err, collapse = "\n  ")
    )
  } else {
    capture_check_cnd(
      check =  nchar(err) == 0,
      file_path = file_path,
      error = FALSE,
      msg_subject = "Task ID/output type/output_type_id combinations",
      msg_attribute = NULL,
      msg_verbs = c("all present.", "missing."),
      details = err
    )
  }

}
