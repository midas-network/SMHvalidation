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
#'@param number2location named vector containing the FIPS as name and the
#'  corresponding location name as value (example: name: "01", value: "Alabama")
#'
#'@details  This function contains 10 tests:
#'\itemize{
#'  \item{Type: }{The projection contains "point" value noted as `type` =
#'  "point".}
#'  \item{Quantile: }{The projection contains "point" value noted as `quantile`
#'  = NA.}
#'  \item{Number of Point: }{The projection contains X point value, X = number
#'  of targets * number of scenarios * number of locations}
#'  \item{Positive Value: }{The projection contains only values superior or
#'  equal to 0, NA will also returns an error}
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
#'  the population size of the corresponding geographical entity (currently
#'  developed on each incidence and cumulative value, starting round 13). The
#'  submission is still accepted if the test failed, but will return a message
#'  asking to verify the projected value for the specific target and location}
#'  \item{Cumulative cases: }{In the submission, the projected cumulative
#'  case counts value should not be lower than the week 0 (or week -1, depending
#'   on availability on the time of submission) of the observed cumulative
#'   cases}
#'  \item{Cumulative deaths: }{In the submission, the projected cumulative
#'  death counts value should not be lower than the week 0 (or week -1,
#'  depending on availability on the time of submission) of the observed
#'  cumulative deaths}
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
#'@importFrom dplyr filter left_join mutate distinct %>%
#'@importFrom tidyr unite
#'@export
test_val <- function(df, pop, last_lst_gs, number2location) {
  # - the data frame has "point" value noted as type = "point" & quantile = NA.
  if (isFALSE(dim(dplyr::filter(df, grepl("point", type)))[1] > 0)) {
    point_test <- paste0("\U000274c Error 501: The data frame is missing point",
                         " value.")
  } else {
    point_test <- NA
  }
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
  # The projection contains X point value, X = number of targets * number of
  # scenarios * number of locations
  if (isFALSE(length(unique(df$target)) * length(unique(df$scenario_id)) *
              length(unique(df$location)) == dim(
                dplyr::filter(df, grepl("point", type)))[1])) {
    all_point <- length(unique(df$target)) *
      length(unique(df$scenario_id)) * length(unique(df$location))
    sub_point <- dim(dplyr::filter(df, grepl("point", type)))[1]
    if (any(grepl("inc inf|prop", df$target))) {
      exp_point <- length(unique(grep("prop|inc inf", unique(df$target),
                                       invert = TRUE))) *
        length(unique(df$scenario_id)) *
        length(unique(df$location))
      sub_exp_point <- dim(dplyr::filter(df, grepl("point", type),
                                         !grepl("inc inf|prop", target)))[1]
      if (isFALSE(exp_point == sub_exp_point)) {
        pointnum_test <-  paste0(
          "\U000274c Error 503: The data frame should contains a 'point' type ",
          "value for each target, scenario and locations projected. Expected ",
          "number of value is: '", exp_point, "' for all required target and ",
          " the data frame  contains: '", sub_exp_point, "' point values.")
      } else {
       # pointnum_test <-  paste0(
       #   "\U0001f7e1 Warning 503: The data frame should contains a 'point'",
       #   " type value for each target, scenario and locations projected. ",
       #   "Expected number of value is: '", all_point,
       #   "' for all targets (required and optionals) and the data frame ",
       #   "contains: '", sub_point, "' point values. The submission will be",
       #   " accepted if point value(s) are missing for Round 14, target ",
       #   "'prop X', scenarios 'A-2022-05-09' and/or 'C-2022-05-09' or for ",
       #   "some locations in Round 14, target 'prop X'.")
        pointnum_test <- NA
      }
    } else {
      pointnum_test <- paste0(
        "\U000274c Error 503: The data frame should contains a 'point' type",
        "  value for each target, scenario and locations projected. Expected ",
        "number of  value is: '", all_point, "' and the data frame contains: '",
        sub_point, "' point values.")
    }
  } else {
    pointnum_test <- NA
  }
  # - all value are positives and contains no NA
  if (isFALSE(all(df$value >= 0))) {
    pointpos_test <- paste0(
      "\U000274c Error 504: All values should be positive, the data frame ",
      "contains negative values.")
  } else {
    if (any(is.na(df$value))) {
      pointpos_test <- paste0(
        "\U000274c Error 504: All values should be positive, the data frame ",
        "contains 'NA' values.")
    } else {
      pointpos_test <- NA
    }
  }
  # - test for all location and target if the projection does not contains only
  #   1 value (for example just 0 incident cases all along the projections)
  test_loc <- grep("66|69|60|74", unique(df$location), invert = TRUE,
                   value = TRUE)
  df3 <- dplyr::filter(df, location %in% test_loc) %>%
    dplyr::mutate(target_name = gsub(".+ wk ahead ", "", target))
  lst_df3 <- split(df3, list(df3$scenario_id, df3$target_name, df3$location))
  pointuniq_test <- lapply(lst_df3, function(x) {
    group <- paste0("target: ", unique(x$target_name), ", location: ",
                    unique(x$location), ", scenario: ", unique(x$scenario_id))
    if (length(unique(x$value)) == 1)  {
      pointuniq_test <- paste0(
        "\U0001f7e1 Warning 505: Some location/target/scenario groups have a ",
        "unique value for the whole projection period. Please verify: ", group)
    } else {
      pointuniq_test <- NA
    }
    return(unique(pointuniq_test))
  })
  # Each group of scenario/location/target has 1 unique point value
  lst_df <- split(df, list(df$scenario_id, df$location, df$target), sep = ";")
  pointone_test <- lapply(seq_along(lst_df), function(x) {
    x_name <- names(lst_df)[x]
    x <- lst_df[[x]]
    group <- paste0("target: ", unique(x$target), ", location: ",
                    unique(x$location), ", scenario: ", unique(x$scenario_id))
    point <- dplyr::filter(x, grepl("point", type))
    if (dim(point)[1] > 1) {
      pointone_test <- paste0(
        "\U000274c Error 506: Each group of scenario, location and target ",
        "should have one unique point value. The group: ", group, " has ",
        dim(point)[1], " points value, please verify")
    } else if (dim(point)[1] < 1) {
      group <-  paste0("target: ", strsplit(x_name, ";")[[1]][3],
                       ", location: ", strsplit(x_name, ";")[[1]][2],
                       ", scenario: ", strsplit(x_name, ";")[[1]][1])
      if (grepl("prop", strsplit(x_name, ";")[[1]][3]) &
          grepl("A-|C-", strsplit(x_name, ";")[[1]][1])) {
        pointone_test <- NA
      } else {
        pointone_test <- paste0(
          "\U000274c Error 506: Each group of scenario, location and target ",
          "should have one unique point value. The group: ", group, " has ",
          dim(point)[1], " points value, please verify")
      }
    } else {
      pointone_test <- NA
    }
    if (grepl("prop|inc inf", group)) {
      pointone_test <- gsub("\U000274c Error", "\U0001f7e1 Warning",
                            pointone_test)
    }
    return(pointone_test)
  })

  if (length(pointone_test) > 100) {
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
      dplyr::select(test, target, location_name, scenario_id, quantile))
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

  # deaths:
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
  # unique projection for each combination
  lst_df <- df %>%
    split(list(df$quantile, df$scenario_id, df$target, df$location), sep = ";")
  valunique_test <- lapply(seq_along(lst_df), function(x){
    x_name <- names(lst_df[x])
    x <- lst_df[[x]]
    if (dim(x)[1] > 1) {
      group <- paste0("target: ", unique(x$target), ", location: ",
                      unique(x$location), ", scenario: ", unique(x$scenario_id),
                      ", quantile:", unique(x$quantile))
      valunique_test <- paste0(
        "\U000274c Error 510: Each quantile/target/scenario/location combination",
        " should have one unique value. Please verify for: ", group)
    } else if (dim(x)[1] < 1) {
      group <-  paste0("target: ", strsplit(x_name, ";")[[1]][3],
                       ", location: ", strsplit(x_name, ";")[[1]][4],
                       ", scenario: ", strsplit(x_name, ";")[[1]][2],
                       ", quantile:", strsplit(x_name, ";")[[1]][1])
      if ((grepl("prop", group) & !is.na(strsplit(x_name, ";")[[1]][1]))) {
        valunique_test <- NA
      } else {
        valunique_test <- paste0(
          "\U000274c Error 510: Each quantile/target/scenario/location combination",
          " should have one unique value. Please verify for: ", group)
      }
    } else {
      valunique_test <- NA
    }
  })
  # Cumulative values are not decreasing
  target_sel <- c("cum death", "cum case", "cum hosp")
  df2 <- dplyr::filter(df, grepl(paste(target_sel, collapse = "|"), target))
  df2 <- dplyr::filter(df2, !is.na(value))
  df2 <-  mutate(df2,
                 location = ifelse(nchar(location) == 1, paste0("0", location),
                                   location),
                 quantile = ifelse(is.na(quantile), "point", quantile),
                 target_name = gsub(".+ wk ahead ", "", target))
  lst_df <- split(df2, list(df2$scenario_id, df2$location, df2$quantile,
                            df2$target_name))
  valcum_test <- lapply(seq_along(lst_df), function(x) {
    group <- paste0("target: ", unique(lst_df[[x]]$target_name), ", location: ",
                    unique(lst_df[[x]]$location), ", scenario: ",
                    unique(lst_df[[x]]$scenario_id), ", quantile: ",
                    lst_df[[x]]$quantile)
    val_test_tot <- NULL
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
  value_test <- na.omit(c(point_test, pointna_test, pointnum_test,
                          pointpos_test, unlist(pointuniq_test),
                          unlist(pointone_test), pointpop_test,
                          valcumcase_test, valcumdeath_test,
                          unlist(valunique_test), unlist(valcum_test)))
  if (length(value_test) == 0)
    value_test <- "No errors or warnings found on Value and Type columns"

  return(value_test)

}
