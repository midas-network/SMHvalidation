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
#'  equal to 0}
#'  \item{Unique Point: }{In the projection, each group of
#'  scenario/location/target has 1 unique point value}
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
    pointnum_test <- paste0(
      "\U000274c Error 503: The data frame should contains a 'point' type value",
      " for each target, scenario and locations projected. Expected number of ",
      "value is: '", length(unique(df$target)) *
        length(unique(df$scenario_id)) * length(unique(df$location)),
      "' and the data frame contains: '", dim(
        dplyr::filter(df, grepl("point", type)))[1], "' point values")
  } else {
    pointnum_test <- NA
  }
  # - all value are positives
  if (isFALSE(all(df$value >= 0))) {
    pointpos_test <- paste0(
      "\U000274c Error 504: All values should be positive, the data frame ",
      "contains negative values.")
  } else {
    pointpos_test <- NA
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
  lst_df <- split(df, list(df$scenario_id, df$location, df$target))
  pointone_test <- lapply(lst_df, function(x) {
    group <- paste0("target: ", unique(x$target), ", location: ",
                    unique(x$location), ", scenario: ", unique(x$scenario_id))
    point <- dplyr::filter(x, grepl("point", type))
    if (dim(point)[1] != 1) {
      pointone_test <- paste0(
        "\U000274c Error 506: Each group of scenario, location and target ",
        "should have one unique point value. The group: ", group, " has ",
        dim(point)[1], " points value, please verify")

    } else {
      pointone_test <- NA
    }
    return(pointone_test)
  })
  # Value should be lower than population size
  if (any(nchar(df$location) == 1)) {
    df$location[which(nchar(df$location) == 1)] <- paste0(
      0, df$location[which(nchar(df$location) == 1)])
  }
  test <- dplyr::left_join(df, pop, by = "location") %>%
    dplyr::mutate(pop_test = ifelse(population < value, 1, 0)) %>%
    dplyr::filter(pop_test > 0)
  if (dim(test)[1] > 0) {
    pointpop_test <-  paste0(
      "\U0001f7e1 Warning 507: Some value(s) are greater than the population ",
      "size. Please verify: ",
      dplyr::distinct(
        dplyr::select(test, target, location_name, scenario_id)) %>%
        tidyr::unite("pop_test_fail", sep = "; ") %>% unlist())
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
      "\U000274c Error 508: Some values are less than the last oberved ",
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
      "\U000274c Error 509: Some values are less than the last oberved ",
      "cumulative deaths count. Please check location(s): ",
      dplyr::distinct(dplyr::select(test, location)))
  } else {
    valcumdeath_test <- NA
  }
  # unique projection for each combination
  lst_df <- df %>%
    split(list(df$quantile, df$scenario_id, df$target, df$location))
  valunique_test <- lapply(lst_df, function(x){
    group <- paste0("target: ", unique(x$target), ", location: ",
                    unique(x$location), ", scenario: ", unique(x$scenario_id),
                    ", quantile:", unique(x$quantile))
    if (dim(x)[1] != 1) {
      valunique_test <- paste0(
        "\U000274c Error 510: Each quantile/target/scenario/location combination",
        " should have one unique value. Please verify for: ", group)
    } else {
      valunique_test <- NA
    }
  })

  value_test <- na.omit(c(point_test, pointna_test, pointnum_test,
                          pointpos_test, unlist(pointuniq_test),
                          unlist(pointone_test), pointpop_test,
                          valcumcase_test, valcumdeath_test,
                          unlist(valunique_test)))
  if (length(value_test) == 0)
    value_test <- "No errors or warnings found on Value and Type columns"

  return(value_test)

}
