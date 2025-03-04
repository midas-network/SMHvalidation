# nocov start
#' Validate `value` format column
#'
#' Validate that all the required type output are present in the submission
#' file and all `value` have the expected format (for example, double greater
#' than 0, etc.)
#'
#' @param df_test data frame to validate
#' @param model_tasks list containing round information for each id columns
#' and model output (type, format, etc.)
#' @param outputtype vector of output type to validate
#' @param n_decimal integer,  number of decimal point accepted in the column
#'  value (only for `"sample"` output type), if `NULL` (default) no limit
#'  expected.
#' @noRd
#'
#' @importFrom dplyr select filter
value_format_test <- function(df_test, model_tasks, outputtype,
                              n_decimal = NULL) {
  # - all value are in the expected format and the column value does not
  # contain any NA
  lapply(outputtype, function(y) {
    sum_test_format <- dplyr::filter(df_test, .data[["output_type"]] == y) |>
      dplyr::select(tidyr::all_of(c("output_type_id"))) |>
      unlist()
    out_type <- unique(unlist(model_tasks$output_type[[y]]$output_type_id))
    if (y %in% c("quantile", "sample")) {
      sum_test_format <- as.numeric(sum_test_format)
    }
    if (isFALSE(all(unique(sum_test_format) %in% out_type)) & y != "sample") {
      message("\U000274c Error: For the type '", y,  "', the ",
              "output_type_id should correspond to: ",
              paste(out_type, collapse = ", "),
              " at least one id is incorrect, please verify")
    }

    value_format <- model_tasks$output_type[[y]]$value
    valtype_test <- ifelse(value_format$type == "integer",
                           all(is_wholenumber(df_test$value)),
                           ifelse(value_format$type == "numeric",
                                  all(is.numeric(df_test$value)),
                                  ifelse(value_format$type == "double",
                                         all(is.double(df_test$value)),
                                         NA)))
    if (isFALSE(valtype_test)) {
      message("\U000274c Error: All values should be '", value_format$type,
              "' the data frame contains some error, please verify.")
    }

    if (y == "sample" && !is.null(n_decimal)) {
      unique_val <- unique(df_test$value)
      unique_val_digit <- abs(unique_val) - floor(abs(unique_val))
      unique_val_digit <- gsub("0\\.|0+$", "", unique_val_digit, perl = TRUE)
      sel_digit_rep <- grepl("9{6,}|0{6,}", unique_val_digit)
      if (any(sel_digit_rep)) {
        unique_val_digit[sel_digit_rep] <-
          gsub("(*)9{6,}.*|(*)0{6,}.*", "\\1", unique_val_digit[sel_digit_rep])
      }
      if (any(unique(nchar(unique_val_digit)) > n_decimal)) {
        message("\U0001f7e1 Warning: All values associated with output type ",
                "'sample' should have a maximum of ", n_decimal,
                " decimal place")
      }
    }

    if (!is.null(value_format$minimum)) {
      min_test <- all(df_test$value >= value_format$minimum)
      if (isFALSE(min_test)) {
        message("\U000274c Error: All values should be greater or equal to ",
                value_format$minimum, " the data frame contains some error,",
                " please verify.")
      }
    }
    if (!is.null(value_format$maximum)) {
      max_test <- all(df_test$value <= value_format$maximum)
      if (isFALSE(max_test)) {
        message("\U000274c Error: All values should be less or equal to ",
                value_format$maximum, " the data frame contains some error, ",
                "please verify.")
      }
    }
    invisible(NULL)
  })
  invisible(NULL)
}

#' Runs Validation Checks on the Projection value and type point columns
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `value` and `type` columns contain the expected information and value.
#'
#'@param df data frame to test
#'@param pop data frame containing the population size of each geographical
#'  entities by fips (in a column "location"), can be `NULL` (not comparison to
#'  population size)
#'@param last_lst_gs list of data frame, named with the corresponding target and
#'  containing the last avaible week of observed data  before start of the
#'  projection, can be `NULL` (not comparison to observed data)
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'@param n_decimal integer,  number of decimal point accepted in the column
#'  value (only for "sample" output type), if NULL (default) no limit expected.
#'
#'@details  This function contains 10 tests:
#' * Required type: All the required type output are present in the
#'  submission file.
#' * Value: All `value` have the expected format (for example, double
#'  greater than 0, etc.).
#' * Numeric Value: All `value` are numeric, `NA` are not accepted).
#' * Unique value: For each target/scenario/location group (except
#'  locations 66 (Guam), 69 (Northern Mariana Island), 60 (American Samoa),
#'  74 (US. Minor Outlying Islands)), the whole time frame projection does not
#'  contain only 1 unique value. For example, contains only 0 or same
#'  cumulative value for the whole projections. The submission is still
#'  accepted if so, but will return a warning asking to verify if the projection
#'  is correct for the specific location/target group.
#' * Value < Population size: (optional) Each projected value cannot
#'  by higher than the population size of the corresponding geographical entity.
#'  The submission is still accepted if the test failed, but will return a
#'  message asking to verify the projected value for the specific target and
#'  location.
#' * Cumulative cases: (optional) If the submission contains cumulative
#'  case count, the projected cumulative case counts value should not be lower
#'  than the week 0 (or week -1, depending on availability on the time of
#'  submission) of the observed cumulative cases.
#' * Cumulative deaths: (optional) If the submission contains cumulative
#'  death, the projected cumulative death counts value should not be lower than
#'  the week 0 (or week -1, depending on availability on the time of submission)
#'   of the observed cumulative deaths.
#' * Unique Projections: In the submission, each model tasks group
#'  (unique combination of `task_ids` columns as specified in the `model_task`
#'  parameter) has one unique value projected. (For example: only 1 value for
#'  quantile 0.5, location US, target 1 wk ahead inc case and scenario A).
#' * Cumulative Projections: For cumulative target only, each model
#'  tasks group associated with a cumulative target has projected values that
#'  increase or stay constant with time.
#'
#' Function called in the `validate_submission()` function.
#'
#'@importFrom dplyr filter summarise n everything distinct select
#'@importFrom tidyr unite all_of
#'@importFrom purrr  map
#'@export
test_val <- function(df, pop, last_lst_gs, model_task, n_decimal = NULL) {

  warning("Function deprecated")

  lapply(model_task, function(x) {
    # Prerequisite
    req_type <- purrr::map(purrr::map(purrr::map(x$output_type, `[`),
                                      "output_type_id"), "required")
    req_type <- req_type[!unlist(purrr::map(req_type, is.null))]
    req_type <- unique(names(req_type))
    outputtype <- names(x$output_type)
    df_test <- dplyr::filter(df, .data[["target"]] %in%
                               unique(unlist(x$task_ids$target)) &
                               .data[["output_type"]] %in% outputtype)
    if (dim(df_test)[1] > 0) {
      # If necessary fix location column to avoid issue
      df_test <- loc_zero(df_test)
      # - all value are in the expected format and the column value does not
      # contain any NA
      value_format_test(df_test, x, outputtype, n_decimal = n_decimal)
      if (isTRUE(any(is.na(df_test$value)))) {
        df_test <- dplyr::filter(df_test, !is.na(.data[["value"]]))
        message("\U000274c Error: All values should be numeric,",
                " the data frame contains 'NA' values.")
      }

      # - unique projection for each combination
      sel_group <- grep("value|model_projection_date|scenario_name",
                        colnames(df_test), invert = TRUE, value = TRUE)
      df_test2 <- dplyr::summarise(df_test, N = dplyr::n(),
                                   .by = tidyr::all_of(sel_group)) |>
        dplyr::filter(.data[["N"]] > 1)
      if (dim(df_test2)[1] > 0) {
        err_groups <- df_test2 |>
          dplyr::select(-.data[["N"]]) |>
          dplyr::distinct() |>
          tidyr::unite("group", dplyr::everything(), sep = ", ") |>
          unlist()
        err_groups <- paste(err_groups[1:5], collapse = "; ")
        message("\U000274c Error: Each group combination",
                " should have one unique value. Please verify: ", err_groups)
      }

      # - required type(s) are present in the submission file
      if (!all(req_type %in% unique(df_test$output_type))) {
        message("\U000274c Error: The data frame is missing at least one ",
                "output. The submission file might contain information output",
                " in the type: ", paste(req_type, collapse = ", "))
      }
    }
    invisible(NULL)
  })
  invisible(NULL)
}
# nocov end
