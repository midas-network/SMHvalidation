#' Runs Validation Checks on the Projection value and type point columns
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `value` and `type` columns contain the expected information and value.
#'
#'@param df data frame to test
#'@param pop data frame containing the population size of each geographical
#'  entities by fips (in a column "location")
#'@param last_lst_gs list of data frame, named with the corresponding target and
#'  containing the last avaible week of observed data  before start of the
#'  projection
#'@param model_task data.frame containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function contains 9 tests:
#'\itemize{
#'  \item{Required type: }{All the required type output are present in the
#'    submission file}
#'  \item{Value: }{All `value` have the expected format (for example, double
#'       greater than 0, etc.)}
#'  \item{Numeric Value: }{All `value` are numeric, `NA` are not accepted)}
#'  \item{Unique value: }{For each target/scenario/location group (except
#'  locations 66 (Guam), 69 (Northern Mariana Island), 60 (American Samoa),
#'  74 (US. Minor Outlying Islands)), the whole time frame projection does not
#'  contain only 1 unique value. For example, contains only 0 or same
#'  cumulative value for the whole projections. The submission is still
#'  accepted if so, but will return a warning asking to verify if the projection
#'   is correct for the specific location/target group}
#'  \item{Value < Population size: }{Each projected value cannot by higher than
#'  the population size of the corresponding geographical entity. The
#'  submission is still accepted if the test failed, but will return a message
#'  asking to verify the projected value for the specific target and location}
#'  \item{Cumulative cases: }{If the submission contains cumulative case count,
#'  the projected cumulative case counts value should not be lower than the
#'   week 0 (or week -1, depending on availability on the time of submission)
#'   of the observed cumulative cases}
#'  \item{Cumulative deaths: }{If the submission contains cumulative death, the
#'  projected cumulative death counts value should not be lower than the week
#'  0 (or week -1, depending on availability on the time of submission) of the
#'  observed cumulative deaths}
#'  \item{Unique Projections: }{In the submission, each group
#'  quantile or sample/horizon/target/scenario/location combination has one
#'  unique value projected. (For example: only 1 value for quantile 0.5,
#'  location US, target 1 wk ahead inc case and scenario A).}
#'  \item{Cumulative Projections: }{In the submission, each
#'  quantile/target name/scenario/location combination for the cumulative target
#'  only, has projected values that increase or stay the same with time.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter left_join mutate distinct %>% arrange matches
#'@importFrom tidyr unite
#'@importFrom purrr discard map
#'@export
test_val <- function(df, pop, last_lst_gs, model_task) {

  # Prerequisite
  req_type <- names(purrr::map(purrr::map(purrr::map(purrr::map(
    model_task$output_types, `[`), "type_id"), "required"), na.omit)) %>%
    unique()
  output_type <- names(model_task$output_types)
  if (any(nchar(df$location) == 1)) {
    df$location[which(nchar(df$location) == 1)] <- paste0(
      0, df$location[which(nchar(df$location) == 1)])
  }
  df <- data.table::data.table(df)

  # - required type(s) are present in the submission file
  if (!all(req_type %in% unique(df[, type]))) {
    type_test <- paste0(
            "\U000274c Error 511: The data frame is missing at least one ",
            "required output. The submission file should contains information",
            " output in the type: ", paste(req_type, collapse = ", "))
  } else {
    type_test <- NA
  }

  # - all value are in the expected format and the column value does not contain
  # any NA
  format_test <- lapply(output_type, function(x) {
    value_format <- model_task$output_types[[x]]$value
    valtype_test <- case_when(
      value_format$type == "double" ~ is.double(df$value),
      value_format$type == "numeric" ~ is.numeric(df$value),
      value_format$type == "integer" ~ is.integer(df$value)
    )
    if (isFALSE(valtype_test)) {
      err_mess <- paste0(
        "\U000274c Error 5041: All values should be '", value_format$type,
        "' the data frame contains some error, please verify.")
    } else {
      err_mess <- NA
    }
    if (!is.null(value_format$minimum)) {
      min_test <- all(df$value >= value_format$minimum)
      if (isFALSE(min_test)) {
        if (is.na(err_mess)) {
          err_mess <- paste0(
            "\U000274c Error 5041: All values should be greater or equal to ",
            value_format$minimum, " the data frame contains some error, please",
            " verify.")
        } else {
          err_mess <- gsub(" the data frame", paste0(
            ", greater or equal to ", value_format$minimum, " the data frame"),
            err_mess)
        }
      }
    }
    if (!is.null(value_format$maximum)) {
      max_test <- all(df$value <= value_format$maximum)
      if (isFALSE(max_test)) {
        if (is.na(err_mess)) {
          err_mess <- paste0(
            "\U000274c Error 5041: All values should be less or equal to ",
            value_format$maximum, " the data frame contains some error, please",
            " verify.")
        } else {
          err_mess <- gsub(" the data frame", paste0(
            ", less or equal to ", value_format$maximum, " the data frame"),
            err_mess)
        }
      }
    }
    return(err_mess)
  })
  if (isTRUE(any(is.na(df$value)))) {
    df <- df[!is.na(value)]
    na_test <- paste0(
      "\U000274c Error 5042: All values should be numeric, the data frame ",
      "contains 'NA' values.")
  } else {
    na_test <- NA
  }

  # - test for all location and target if the projection does not contains only
  #   1 value (for example just 0 incident cases all along the projections)
  test_loc <- grep("66|69|60|74", unique(df$location), invert = TRUE,
                   value = TRUE)
  df_loc <- df[location %in% test_loc]
  sel_group <- c(grep("horizon|target_end_date", names(model_task$task_ids),
                      value = TRUE, invert = TRUE), "type", "type_id")
  df_loc[, var := var(value), by = sel_group]
  df_loc <- df_loc[var == 0]
  if (dim(df_loc)[1] > 0) {
    err_groups <- df_loc %>% dplyr::select(-var, -value) %>%
      dplyr::distinct() %>% tidyr::unite("group", dplyr::everything(),
                                         sep = ", ") %>% unlist()
    pointuniq_test <-  paste0(
      "\U0001f7e1 Warning 505: Some location/target/scenario groups have a ",
    "unique value for the whole projection period. Please verify: ", err_groups)
  } else {
    pointuniq_test <- NA
  }
  if (length(na.omit(unlist(pointuniq_test))) > 100) {
    pointuniq_test <- paste0(unique(unlist(purrr::map(strsplit(
      pointuniq_test, "Please verify: "), 1))), length(pointuniq_test),
      " unique groups have been identified with this issue. For example: \n",
      paste("group: ", head(purrr::map(strsplit(pointuniq_test, "verify: "), 2),
                            3), collapse = ";\n"), "; \netc.")
  }

   # - Value should be lower than population size
  test <- dplyr::left_join(df, pop, by = "location") %>%
    dplyr::mutate(pop_test = ifelse(population < value, 1, 0)) %>%
    dplyr::filter(pop_test > 0)
  if (dim(test)[1] > 0) {
    test <- dplyr::distinct(
      dplyr::select(test, target, location_name, scenario_id,
                    matches("quantile|sample")))
    if (dim(test)[1] > 100)
      test <- dplyr::distinct(
        dplyr::select(test, target, location_name, scenario_id))
    if (dim(test)[1] > 100)
      test <- dplyr::distinct(dplyr::select(test, scenario_id, location_name))
    pointpop_test <-  paste0(
      "\U0001f7e1 Warning 507: Some value(s) are greater than the population ",
      "size. Please verify: ",
      tidyr::unite(test, "pop_test_fail", sep = "; ") %>% unlist())
  } else {
    pointpop_test <- NA
  }

  # - Cumulative value should not be lower than GS cumulative data
  # (deaths; cases only)
  cum_gs <- last_lst_gs[grepl("cumulative", names(last_lst_gs))]
  ## Cases:
  if (!is.null(cum_gs$confirmed_cumulative_num)) {
    df_cum_case <- dplyr::filter(df, grepl("cum case", target))
    test <- dplyr::left_join(df_cum_case, cum_gs$confirmed_cumulative_num,
                             by = "location") %>%
      dplyr::mutate(cum_test = ifelse(last_value - value > last_value * 0.05,
                                      1, 0)) %>%
      dplyr::filter(cum_test > 0)

    if (dim(test)[1] > 0) {
      valcumcase_test <- paste0(
        "\U000274c Error 508: Some values are less than the last observed ",
        "cumulative cases count. Please check location(s): ", dplyr::distinct(
          dplyr::select(test, location)))
    } else {
      valcumcase_test <- NA
  }
  } else {
    valcumcase_test <- NA
  }
  ## Deaths:
  if (!is.null(cum_gs$deaths_cumulative_num)) {
    df_cum_death <- dplyr::filter(df, grepl("cum death", target))
    test <- dplyr::left_join(df_cum_death, cum_gs$deaths_cumulative_num,
                             by = "location") %>%
      dplyr::mutate(cum_test = ifelse(last_value - value > last_value * 0.05,
                                      1, 0)) %>%
      dplyr::filter(cum_test > 0)

    if (dim(test)[1] > 0) {
      valcumdeath_test <- paste0(
        "\U000274c Error 509: Some values are less than the last observed ",
        "cumulative deaths count. Please check location(s): ",
        dplyr::distinct(dplyr::select(test, location)))
    } else {
      valcumdeath_test <- NA
    }
  } else {
    valcumdeath_test <- NA
  }

  # - unique projection for each combination
  sel_group <- grep(
    "value|model_projection_date|scenario_name",
    colnames(df), invert = TRUE, value = TRUE)
  df_test <- df[, .N, by = sel_group]
  df_test <- df_test[N > 1]
  if (dim(df_test)[1] > 0) {
    err_groups <- df_test %>% dplyr::select(-N) %>%
      dplyr::distinct() %>% tidyr::unite("group", dplyr::everything(),
                                         sep = ", ") %>% unlist()
    valunique_test <-  paste0(
      "\U000274c Error 510: Each group combination",
      " should have one unique value. Please verify: ", err_groups)
  } else {
    valunique_test <- NA
  }
  if (length(na.omit(unlist(valunique_test))) > 100) {
    valunique_test <- paste0(unique(unlist(purrr::map(strsplit(
      valunique_test, "Please verify: "), 1))), length(valunique_test),
      " unique groups have been identified with this issue. For example: \n",
      paste("group: ", head(purrr::map(strsplit(valunique_test, "verify: "), 2),
                            3), collapse = "; \n"), "; \netc.")
  }

  # - Cumulative values are not decreasing
  target_sel <- unique(unlist(data.table::data.table(
    model_task$target_metadata[[1]])[grepl("cumulative", description,
                                           ignore.case = TRUE)][,"target_id"]))
  if (length(target_sel) > 0) {
    df_cum <- df[grepl(paste(target_sel, collapse = "|"), target)]
    sel_group <- grep(
      "value|target_end_date|model_projection_date|scenario_name|horizon",
      names(df_cum), invert = TRUE, value = TRUE)
    df_cum[ , diff := value - data.table::shift(value, 1, type = "lag"),
            by = sel_group]
    df_cum <- df_cum[diff < 0]
    if (dim(df_test)[1] > 0) {
      err_groups <- df_cum %>% dplyr::select(-diff, -value) %>%
        dplyr::distinct() %>% tidyr::unite("group", dplyr::everything(),
                                           sep = ", ") %>% unlist()
      valcum_test <- paste0(
        "\U000274c Error 511: The cumulative values are decreasing, please ",
        "verify the group: ", err_groups)
    } else {
      valcum_test <- NA
    }
    if (length(na.omit(unlist(valcum_test))) > 100) {
      valcum_test <- paste0(unique(unlist(purrr::map(strsplit(
        valcum_test, "please verify "), 1))), length(valcum_test),
        " unique groups have been identified with this issue. For example: \n",
        paste(head(purrr::map(strsplit(valcum_test, "verify the "), 2),
                   3), collapse = ";\n"), "; \netc.")
    }
  } else {
    valcum_test <- NA
  }

  value_test <-  na.omit(c(type_test, unlist(format_test), na_test,
                           pointuniq_test, pointpop_test, valcumcase_test,
                           valcumdeath_test, valunique_test, valcum_test))
  value_test <- unique(value_test)
  if (length(value_test) == 0)
    value_test <- "No errors or warnings found on Value and Type columns"
  return(value_test)

}
