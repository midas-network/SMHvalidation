#' Runs Validation Checks on the Target and target_end_date columns
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `target` and `target_end_date` columns contain the expected information
#' and value.
#'
#'@param df data frame to test
#'@param round numeric corresponding to the current round number
#'@param start_date corresponds to the "1 wk ahead" target in the projection
#'  file
#'
#'@details  This functions contains 7 tests:
#'\itemize{
#'  \item{"Target name": }{The target should correspond to the target name as
#'  expressed in the SMH Github: "inc death", "inc case", "cum death",
#'  "cum case", "inc hosp", "cum hosp".}
#'  \item{"Target number": }{The submission file contains projection for all the
#'  targets. It is accepted to submit only a subset of target but a warning
#'  message will be return. }
#'  \item{"End of week": }{The date in the "target_end_date" column should always
#'  correspond to the end of the epiweek (Saturday).}
#'  \item{"Number of week projected": }{The submission file contains projections
#'  for the expected number of weeks or more. If a team submits more week than
#'  expected, a warning message will be returned, but the submission will be
#'  accepted. However, an error message will be returned if the submission file
#'  contains less projection week than expected.}
#'  \item{"Week projected": }{The submission file contains projection for all
#'  expected weeks for each target, location, scenario and quantiles
#'  combination.}
#'  \item{"Starting date": }{The 1 week ahead of the `target_end_date` starts on
#'  the expected date (end of the epiweek of the starting projection date).}
#'  \item{"Correct date": }{Each target_end_date corresponds to the expected date
#'   for example if 1 wk ahead = 2022-01-15, than 2 wk ahead = 2022-01-22.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr %>% mutate filter
#'@importFrom lubridate wday as.period
#'@export
test_target <- function(df, start_date, round) {
  # - target names (should be the same as in the GitHub)
  target_names <- c("inc death", "inc case", "cum death", "cum case",
                    "inc hosp", "cum hosp")
  if (isFALSE(all(gsub(".+ wk ahead ", "", df$target) %in% target_names))) {
    targetname_test <-  paste0(
      "\U000274c Error: At least one of the target_names is misspelled. ",
      "Please verify, the target_names should be: '",
      paste(target_names, collapse = ", "), "'. The data frame contains: '",
      paste(unique(gsub(".+ wk ahead ", "", df$target)), collapse = ", "),
      "', as targets names.")
  } else {
    targetname_test <- NA
  }
  # - the submission contains all the targets. It is also accepted
  # to submit projections for only certain target (for example: only cases, etc.)
  if (isFALSE(length(unique(gsub(".+ wk ahead ", "", df$target))) == 6)) {
    targetnum_test <- paste0(
      "\U0001f7e1 Warning: The data frame contains projections for ",
      length(unique(gsub(".+ wk ahead ", "", df$target))), " targets instead ",
      "of 6. '", paste(unique(gsub(".+ wk ahead ", "", df$target)),
                       collapse = ", "), "' have been submitted.")
  } else {
    targetnum_test <- NA
  }
  # - target_end_date corresponds to the end of the epiweek (Saturday)
  if (isFALSE(all(unique(df$target_end_date) %>% lubridate::wday() %in% 7))) {
    targetday_test <- paste0(
      "\U000274c Error: The target_end_date should correspond to the end of ",
      "the epiweek (Saturday). For example, if the 1st week projection is on ",
      "the week starting 2021-11-14 and ending 2021-11-20. The target_end_week",
      " for this week should be: 2021-11-20.")
  } else {
    targetday_test <- NA
  }
  # - target_end_date projects for the expected number of weeks or more
  #  (if a team submit more we will still accept it)
  if (round > 9) {
    if (round %in%  c(11, 12)) {
      n_target_week <- 12 # minimum number of projected week accepted
      max_week <- 12 # maximum number of projected week accepted
    } else {
      n_target_week <- 26 # minimum number of projected week accepted
      max_week <- 52 # maximum number of projected week accepted
    }
  } else {
    n_target_week <- 13 # minimum number of projected week accepted
    max_week <- 26 # maximum number of projected week accepted
  }
  if (isFALSE(length(unique(df$target_end_date)) >= n_target_week)) {
    targetweek_test <- paste0(
      "\U000274c Error: The projections should contains at least ",
      n_target_week, " weeks of projection. The data frame contains only: ",
      length(unique(df$target_end_date)), " week(s).")
  } else {
    if (isTRUE(length(unique(df$target_end_date)) > max_week)) {
      targetweek_test <- paste0(
        "\U0001f7e1 Warning: The projection contains more projected week than ",
        "expected.")
    } else {
      targetweek_test <- NA
    }}
  # - all target weeks are present (1,2,3, etc.) by target, scenario, location,
  # quantile
  df2 <-  dplyr::mutate(df, target_name = gsub(".+ wk ahead ", "", df$target))
  lst_df <- split(df2, list(df2$scenario_id, df2$location, df2$quantile,
                            df2$target_name))
  targetwnum_test <- lapply(lst_df, function(x) {
    if (isFALSE(all(sort(as.numeric(unique(gsub("[^[:digit:]]", "",
                                                x$target)))) ==
                    seq_len(length(unique(x$target_end_date)))))) {
      group <- paste0("target: ", unique(x$target_name), ", location: ",
                      unique(x$location), ", quantile: ",unique(x$quantile),
                      ", scenario: ",unique(x$scenario_id))
      targetwnum_test <- paste0(
        "\U000274c Error: At least one target week is missing in the time ",
        "series. Please verify: ", group)
    } else {
      targetwnum_test <- NA
    }
  })
  # - start on the good start date
  if (isFALSE(unique(dplyr::filter(df, grepl("^1 wk ahead", target))
                     [, "target_end_date", TRUE]) == as.Date(start_date))) {
    targetstart_test <- paste0(
      "\U000274c Error: 1st week target end date is not valid. It should be: '",
      start_date, "' but it is: '", unique(dplyr::filter(
        df, grepl("^1 wk ahead", target))[, "target_end_date", TRUE]), "'.")
  } else {
    targetstart_test <- NA
  }
  # - contains all the correct time date
  df2 <- df %>% dplyr::mutate(start_date_t = as.Date(target_end_date) -
                                lubridate::as.period(as.numeric(gsub(
                                  "[^[:digit:]]", "", target)), "week") + 7) %>%
    dplyr::filter(start_date_t != start_date)
  if (dim(df2)[1] != 0) {
    wk_error <- as.numeric(unique(gsub("[^[:digit:]]", "", df2$target)))
    expected <- lapply(wk_error, function(x) {
      paste0("For ", x, " wk ahead, the target_end_date should be: ",
             start_date + lubridate::period(x, "week") - 7)
    }) %>% unlist()
    targetalldate_test <- paste0(
      "\U000274c Error: One or multiple target(s) end date are not valid. ",
      expected)
  } else {
    targetalldate_test <- NA
  }

  target_test <- na.omit(c(targetname_test,  targetnum_test, targetday_test,
                           targetweek_test, unlist(targetwnum_test),
                           targetstart_test, targetalldate_test))
  if (length(target_test) == 0)
    target_test <- paste0("No errors or warnings found in target and ",
                          "target_end_date columns")

  return(target_test)
}

