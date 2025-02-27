#'@importFrom dplyr filter mutate left_join lag arrange distinct everything
#'@importFrom tidyr all_of unite
cumul_value_test <- function(df, checks, obs, file_path) {
  # - Cumulative value should not be lower than GS cumulative data
  # (deaths; cases only)
  if (any(grepl("cum", df$target))) {
    cum_obs <- dplyr::filter(obs, grepl("cum ", .data[["target"]]))
    df_h1 <- dplyr::filter(df, grepl("cum ", .data[["target"]]),
                           .data[["horizon"]] == 1)
    test <- dplyr::left_join(df_h1, cum_obs, by = "location") |>
      dplyr::mutate(dff = ifelse(.data[["observation"]] - .data[["value"]] >
                                   (.data[["observation"]] * 0.05), 1, 0)) |>
      dplyr::filter(.data[["dff"]] > 0)
    check$cumul_value <-
      capture_check_cnd(!(dim(test)[1] > 0), file_path, error = TRUE,
                        msg_verbs = c("are", "are not"), details = msg_dt,
                        msg_subject = "{.var value} at {.var horizon} 1",
                        msg_attribute = paste0("equal or greater than the last",
                                               " observed cumulative count"))

    df_cum <- dplyr::filter(df, grepl("cum ", .data[["target"]]))
    df_cum <-
      dplyr::arrange(df_cum,
                     dplyr::across(tidyr::all_of(c("target", "horizon"))))
    sel_group <- grep(paste0("value|target_end_date|model_projection_date",
                             "|scenario_name|horizon"), names(df_cum),
                      invert = TRUE, value = TRUE)
    df_cum <-
      dplyr::mutate(df_cum,
                    diff = .data[["value"]] - dplyr::lag(.data[["value"]]),
                    .by = tidyr::all_of(sel_group)) |>
      dplyr::filter(diff < 0)
    msg_dt <- NULL
    if (!dim(df_cum)[1] > 0) {
      err_groups <- df_cum %>%
        dplyr::select(-tidyr::all_of(c("diff", "value"))) %>%
        dplyr::distinct() %>%
        tidyr::unite("group", dplyr::everything(), sep = ", ") %>%
        unlist()
      msg_dt <- paste0("Please verify the group: ",
                       paste(err_groups[1:5], collapse = "; "))
    }
    check$cumul_proj <-
      capture_check_cnd(!dim(df_cum)[1] > 0, file_path, error = TRUE,
                        msg_verbs = c("are not", "are"), details = msg_dt,
                        msg_subject = "The cumulative values",
                        msg_attribute = "decreasing.")
  }
  return(checks)
}

#'@importFrom hubValidations capture_check_cnd
#'@importFrom dplyr filter mutate select distinct everything left_join
#'@importFrom tidyr all_of unite
#'@importFrom stats var
value_test <- function(df, checks, file_path, n_decimal = NULL, pop = NULL,
                       obs = NULL) {

  unique_val <- unique(df$value)

  # Test for number of decimal
  if (any(grepl("sample", df$output_type)) && !is.null(n_decimal)) {
    unique_val_digit <- abs(unique_val) - floor(abs(unique_val))
    unique_val_digit <- gsub("0\\.|0+$", "", unique_val_digit, perl = TRUE)
    sel_digit_rep <- grepl("9{6,}|0{6,}", unique_val_digit)
    if (any(sel_digit_rep)) {
      unique_val_digit[sel_digit_rep] <-
        gsub("(*)9{6,}.*|(*)0{6,}.*", "\\1", unique_val_digit[sel_digit_rep])
    }
    n_dec_check <- all(unique(nchar(unique_val_digit)) <= n_decimal)
    msg_dt <- paste0("A maximum of ", n_decimal, " decimal place is expected.")
    checks$n_decimal <-
      capture_check_cnd(n_dec_check, file_path, error = FALSE,
                        msg_verbs = c("is", "is not"),
                        msg_subject = paste0("{.var value} associated with ",
                                             "{.var sample}"),
                        details = msg_dt, msg_attribute = "in a valid format.")
  }

  # - test for NA value
  checks$na_value <-
    capture_check_cnd(!any(is.na(unique_val)), file_path, error = TRUE,
                      msg_verbs = c("does not contain", "contains"),
                      msg_subject = "{.var value}",
                      msg_attribute = "`NA` value.")


  # - test for all location and target if the projection does not contains
  # only 1 value (for example just 0 incident cases all along the
  #  projections)
  test_loc <- grep("66|69|60|74", unique(df$location), invert = TRUE,
                   value = TRUE)
  df_loc <- dplyr::filter(df, .data[["location"]] %in% test_loc)
  sel_group <- c(grep("horizon|target_end_date|value", names(df_loc),
                      value = TRUE, invert = TRUE))
  df_loc <- dplyr::mutate(df_loc, var = stats::var(.data[["value"]]),
                          .by = tidyr::all_of(sel_group)) |>
    dplyr::filter(.data[["var"]] == 0)
  msg_dt <- NULL
  if (!(dim(df_loc)[1] > 0)) {
    err_groups <- df_loc |>
      dplyr::select(-tidyr::all_of(c("var", "value", "horizon"))) |>
      dplyr::distinct() |>
      tidyr::unite("group", dplyr::everything(), sep = ", ") |>
      unlist()
    err_groups <- err_groups[1:5]
    msg_dt <- paste0("Please verify, for example: ",
                     paste(err_groups, collapse = "; "))
  }
  check$flat_projection <-
    capture_check_cnd(!dim(df_loc)[1] > 0, file_path, error = FALSE,
                      msg_verbs = c("have not", "have"), details = msg_dt,
                      msg_subject = "Some time series projections",
                      msg_attribute = paste0("a unique value for the whole ",
                                             "projection period."))
  # - Value should be lower than population size
  if (!is.null(pop)) {
    test <- dplyr::left_join(df, pop, by = "location") |>
      dplyr::mutate(pop_test = ifelse(.data[["population"]] < .data[["value"]],
                                      1, 0)) |>
      dplyr::filter(.data[["pop_test"]] > 0)
    msg_dt <- NULL
    if (!(dim(test)[1] > 0)) {
      err_groups <-
        dplyr::distinct(dplyr::select(test,
                                      tidyr::all_of(c("location_name")))) |>
        tidyr::unite("group", dplyr::everything(), sep = ", ") |>
        unlist()
      msg_dt <- paste0("Please verify: ", paste(err_groups, collapse = "; "))
    }
    check$flat_projection <-
      capture_check_cnd(!dim(test)[1] > 0, file_path, error = TRUE,
                        msg_verbs = c("All {.var value} are equal or lower",
                                      "Some {.var value} are greater"),
                        details = msg_dt, msg_subject = "",
                        msg_attribute = "than the population size.")

    if (!is.null(obs)) checks <- cumul_value_test(df, checks, obs, file_path)
  }
  return(checks)
}
