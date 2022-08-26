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
#'@param js_def list containing round definitions: number and names of columns,
#' target names, ...
#'
#'@details  This function contains 12 tests:
#'\itemize{
#'  \item{Type: }{The projection contains "point" value noted as `type` =
#'  "point" when required.}
#'  \item{Quantile: }{The projection contains "point" value noted as `quantile`
#'  = NA.}
#'  \item{Number of Point: }{The projection contains X point value, X = number
#'  of targets * number of scenarios * number of locations (only when all
#'  required targets required point projections for all scenarios, locations,
#'  etc.)}
#'  \item{Value: }{All `value` have the expected value (for example,
#'  ">= 0" or in between 0 and 1)}
#'  \item{Numeric Value: }{All `value` are numeric, `NA` are not accepted)}
#'  \item{Unique Point: }{In the projection, each group of
#'  scenario/location/target has 1 unique point value (for the required targets)}
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
#'  \item{Unique Projections: }{In the submission, each
#'  quantile/target/scenario/location combination has one unique value
#'  projected. (For example: only 1 value for quantile 0.5, location US,
#'  target 1 wk ahead inc case and scenario A).}
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
test_val <- function(df, pop, last_lst_gs, js_def) {

  # Target type information
  target_type <- purrr::map(c(js_def$targets$required, js_def$targets$optional),
                            "type")
  target_type <-purrr::discard(target_type, is.null)

  # - the data frame has "point" value when required
  if (any(grepl("point", purrr::map(target_type, "required")))) {
    name_target <- names(purrr::keep(purrr::map(target_type, "required"),
                                     function(x) any(x %in% "point")))
    df2 <- dplyr::filter(df, grepl(paste(name_target, collapse = "|"), target))
    if (isFALSE(dim(dplyr::filter(df2, grepl("point", type)))[1] > 0)) {
      point_test <- paste0(
        "\U000274c Error 501: The data frame is missing point value.")
    } else {
      point_test <- NA
    }
  } else {
    point_test <- NA
  }

  # Verify all points value are correctly identified
  if (any(any(grepl("all|point", unlist(target_type))))) {
    if (isFALSE(all(is.na(dplyr::filter(df, grepl("point", type))$quantile)))) {
      pointna_test <- paste0(
        "\U000274c Error 502: The value type 'point' should have NA as ",
        "'quantile'. The data frame contains: '",
        na.omit(dplyr::filter(df, grepl("point", type))$quantile),
        "' in the column 'quantile' for 'point' value.")
      pointna_test <- unique(pointna_test)
    } else {
      pointna_test <- NA
    }
  } else {
    pointna_test <- NA
  }

  # The projection contains X point value, X = number of targets * number of
  # scenarios * number of locations [only when all required targets required
  # point projection for all scenarios, locations]
  if (all(
    grepl("all", map(map(js_def$targets$required, "scenarios"), "required")) &
    grepl("all", map(map(js_def$targets$required, "location"), "required")) &
    grepl("point", map(map(js_def$targets$required, "type"), "required")))) {
    df2 <- dplyr::filter(df, grepl(paste(names(js_def$targets$required),
                                         collapse = "|"), target))
    if (isFALSE(length(unique(df2$target)) * length(unique(df2$scenario_id)) *
                length(unique(df2$location)) == dim(
                  dplyr::filter(df2, grepl("point", type)))[1])) {
      all_point <- length(unique(df2$target)) *
        length(unique(df2$scenario_id)) * length(unique(df2$location))
      sub_point <- dim(dplyr::filter(df2, grepl("point", type)))[1]
        if (isFALSE(all_point == sub_point)) {
          pointnum_test <-  paste0(
            "\U000274c Error 503: The data frame should contains a 'point' type ",
            "value for each target, scenario and locations projected. Expected ",
            "number of value is: '", all_point, "' for all required target and ",
            " the data frame  contains: '", sub_point, "' point values.")
        } else {
          pointnum_test <- NA
        }
    } else {
      pointnum_test <- NA
      }
  } else {
    pointnum_test <- NA
  }

  # - all value are positives and contains no NA
  all_target <- names(c(js_def$targets$required, js_def$targets$optional))
  all_target <- grep("^$", all_target, value = TRUE, invert = TRUE)
  value_targ_test <- lapply(all_target, function(x) {
    test <- c(js_def$targets$required, js_def$targets$optional)[[x]]$value
    spl_test <- strsplit(test, " ")
    df_test <- dplyr::filter(df, grepl(x, target))
    value_targ_test <- NULL
    for (i in seq_along(test)) {
      df_res <-  df_test[!eval(call(spl_test[[i]][1], df_test$value,
                                    as.numeric(spl_test[[i]][2]))), ]
      if (dim(df_res)[1] > 0) {
      val_test <- paste0(
          "\U000274c Error 5041: All values should be ", test[[i]], " for the ",
          "target: ", x, ", the data frame contains some error, please verify.")
      } else {
        val_test <- NA
      }
      value_targ_test <- c(value_targ_test, val_test)
    }
    value_targ_test <- unique(value_targ_test)
    return(value_targ_test)
  })

  if (any(is.na(df$value))) {
    pointnumeric_test <- paste0(
      "\U000274c Error 5042: All values should be numeric, the data frame ",
      "contains 'NA' values.")
  } else {
    pointnumeric_test <- NA
  }

  # - test for all location and target if the projection does not contains only
  #   1 value (for example just 0 incident cases all along the projections)
  test_loc <- grep("66|69|60|74", unique(df$location), invert = TRUE,
                   value = TRUE)
  df3 <- dplyr::filter(df, location %in% test_loc) %>%
    dplyr::mutate(target_name = gsub(".+ wk ahead ", "", target))
  sel_group <- grep(
    "value|target_end|target$|type|quantile|model_projection_da|scenario_name",
    names(df3), invert = TRUE, value = TRUE)
  lst_df3 <-  split(df3, as.list(df3[,sel_group]), drop = TRUE)
  lst_df3 <- purrr::discard(lst_df3, function(x) dim(x)[[1]] < 1)
  pointuniq_test <- lapply(lst_df3, function(x) {
    group <- paste(names(unique(x[, sel_group])), ":", unique(x[, sel_group]),
                   collapse = ", ")
    if (length(unique(x$value)) == 1)  {
      pointuniq_test <- paste0(
        "\U0001f7e1 Warning 505: Some location/target/scenario groups have a ",
        "unique value for the whole projection period. Please verify: ", group)
    } else {
      pointuniq_test <- NA
    }
    return(unique(pointuniq_test))
  })

  # Each required group of scenario/location/target has 1 unique point value
  name_target <- names(purrr::keep(purrr::map(target_type, "required"),
                                   function(x) any(x %in% "point")))
  if (length(name_target) > 0) {
    print("wrong")
    df2 <- dplyr::filter(df, grepl(paste(name_target, collapse = "|"), target))
    sel_group <- grep(
      "value|target_end_date|type|quantile|model_projection_date|scenario_name",
      js_def$column_names, invert = TRUE, value = TRUE)
    lst_df <-  split(df2, as.list(df2[,sel_group]), drop = TRUE)
    lst_df <- purrr::discard(lst_df, function(x) dim(x)[[1]] < 1)
    pointone_test <- lapply(seq_along(lst_df), function(x) {
      x <- lst_df[[x]]
      group <- paste(names(unique(x[, sel_group])), ":", unique(x[, sel_group]),
                     collapse = ", ")
      point <- dplyr::filter(x, grepl("point", type))
      if (dim(point)[1] != 1) {
        pointone_test <- paste0(
          "\U000274c Error 506: Each group of scenario, location and target ",
          "should have one unique point value. The group: ", group, " has ",
          dim(point)[1], " points value, please verify")
      } else {
        pointone_test <- NA
      }
      if (length(names(js_def$targets$optional)) > 0) {
        if (grepl(paste(names(js_def$targets$optional), collapse = "|"), group)) {
          pointone_test <- gsub("\U000274c Error", "\U0001f7e1 Warning",
                                pointone_test)
        }
      }
      return(pointone_test)
    })
  } else {
    pointone_test <- NA
  }
  if (length(unique(na.omit(unlist(pointone_test)))) > 100) {
    pointone_test <- unique(gsub("\\d+? wk ahead ", "", unlist(pointone_test)))
    pointone_test <- unique(gsub("has \\d+? points value", "has some issue(s)",
                                 pointone_test))
  }

  # Value should be lower than population size
  if (any(nchar(df$location) == 1)) {
    df$location[which(nchar(df$location) == 1)] <- paste0(
      0, df$location[which(nchar(df$location) == 1)])
  }
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

  # Cumulative value should not be lower than GS cumulative data (deaths; cases)
  cum_gs <- last_lst_gs[grepl("cumulative", names(last_lst_gs))]

  # cases:
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

  # deaths:
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

  # unique projection for each combination
  sel_group <- grep(
    "value|target_end_date|type|model_projection_date|scenario_name",
    js_def$column_names, invert = TRUE, value = TRUE)
  lst_df <-  split(df, as.list(df[,sel_group]), drop = TRUE)
  lst_df <- purrr::discard(lst_df, function(x) dim(x)[[1]] < 1)
  valunique_test <- lapply(seq_along(lst_df), function(x){
    x <- lst_df[[x]]
    if (dim(x)[1] > 1) {
      group <- paste(names(unique(x[, sel_group])), ":", unique(x[, sel_group]),
                     collapse = ", ")
      valunique_test <- paste0(
        "\U000274c Error 510: Each group combination",
        " should have one unique value. Please verify for: ", group)
    } else {
      valunique_test <- NA
    }
  })
  if (length(na.omit(unlist(valunique_test))) > 100) {
    valunique_test <- unique(na.omit(unlist(valunique_test))) %>%
      gsub(" target : \\d{1,2} wk ahead ", " target: ", .) %>% unique
  }

  # Cumulative values are not decreasing
  target_sel <- grep("cum |peak time hosp",
                     names(js_def$targets$required), value = TRUE)
  df2 <- dplyr::filter(df, grepl(paste(target_sel, collapse = "|"), target))
  ## automatic fix location to avoid issue and error message
  df2 <- dplyr::filter(df2, !is.na(value))
  df2 <- dplyr::mutate(
    df2, location = ifelse(nchar(location) == 1, paste0("0", location),
                           location),
    target_name = gsub(".+ wk ahead ", "", target))
  if (any(grepl("quantile", colnames(df2)))) {
    df2 <- dplyr::mutate(df2, quantile = ifelse(is.na(quantile), "point",
                                                quantile))
  }
  sel_group <- grep(
    "value|target_end_date|type|model_projection_date|scenario_name|target$",
    names(df2), invert = TRUE, value = TRUE)
  lst_df <-  split(df2, as.list(df2[,sel_group]), drop = TRUE)
  lst_df <- purrr::discard(lst_df, function(x) dim(x)[[1]] < 2)
  valcum_test <- lapply(seq_along(lst_df), function(x) {
    group <- paste(names(unique(lst_df[[x]][, sel_group])), ":",
                   unique(lst_df[[x]][, sel_group]),
                   collapse = ", ")
    val_test_tot <- NULL
    lst_df[[x]] <- dplyr::arrange(lst_df[[x]], target_end_date)
    for (i in 1:(dim(lst_df[[x]])[1] - 1)) {
      n_val <- lst_df[[x]]$value[i]
      n1_val <- lst_df[[x]]$value[i + 1]
      if (n1_val < n_val) {
        val_test <- paste0(
          "\U000274c Error 511: The cumulative value of the target ",
          lst_df[[x]]$target[i + 1], ", is lower than the previous week target",
          ", please verify the group: ", group)
      } else {
        val_test <- NA
      }
      val_test <- unique(val_test)
      val_test_tot <- unique(c(val_test_tot, val_test))
    }
    return(val_test_tot)
  })
  if (length(na.omit(unlist(valcum_test))) > 100) {
    valcum_test <- unique(na.omit(unlist(valcum_test))) %>%
      gsub(" quantile : (\\d?(\\.\\d+)?|point),", "", .) %>% unique()
  }

  value_test <- na.omit(c(point_test, pointna_test, pointnum_test,
                          unlist(value_targ_test), pointnumeric_test,
                          unlist(pointuniq_test),
                          unlist(pointone_test), pointpop_test,
                          valcumcase_test, valcumdeath_test,
                          unlist(valunique_test), unlist(valcum_test)))
  value_test <- unique(value_test)
  if (length(value_test) == 0)
    value_test <- "No errors or warnings found on Value and Type columns"

  return(value_test)

}
