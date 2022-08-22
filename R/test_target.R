#' Runs Validation Checks on the Target and target_end_date columns
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `target` and `target_end_date` columns contain the expected information
#' and value.
#'
#'@param df data frame to test
#'@param start_date corresponds to the "1 wk ahead" target in the projection
#'  file
#'@param js_def list containing round definitions: number and names of columns,
#' target names, ...
#'
#'@details  This function contains 9 tests:
#'\itemize{
#'  \item{Target name: }{The target should correspond to the target name as
#'  expressed in the SMH Github. }
#'  \item{Target number: }{The submission file contains projection for all
#'  required targets. It is accepted to submit only a subset of target but
#'  a warning message will be return (only if a required target is missing.}
#'  \item{End of week: }{The date in the "target_end_date" column should always
#'  correspond to the end of the epiweek (Saturday).}
#'  \item{Number of week projected: }{The submission file contains projections
#'  for the expected number of weeks or more. If a team submits more week than
#'  expected, a warning message will be returned, but the submission will be
#'  accepted. However, an error message will be returned if the submission file
#'  contains less projection week than expected. Starting round 13, if the file
#'   contains less projected weeks than expected, the submission will still be
#'   accepted, but will return a warning message and might not be included
#'   in the Ensembles}
#'  \item{Week projected: }{The submission file contains projection for all
#'  expected weeks for each target, location, scenario (age_group) and quantiles
#'  or sample combination.}
#'  \item{Starting date: }{The 1 week ahead of the `target_end_date` starts on
#'  the expected date (end of the epiweek of the starting projection date).}
#'  \item{Correct date: }{Each target_end_date corresponds to the expected date
#'   for example if 1 wk ahead = 2022-01-15, than 2 wk ahead = 2022-01-22.}
#'  \item{Target value: }{For the round 14s and 15, and for the optional target
#'  "prop X", the associated value with this target should be between 0 and 1
#'  and should be noted with quantile = NA and type = "point".}
#'  \item{NA target}{The projection contains NA for all "target_end_date"
#'  for the target requiring no time series information. }
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr %>% mutate filter
#'@importFrom lubridate wday as.period period
#'@importFrom purrr map keep
#'@export
test_target <- function(df, start_date, js_def) {
  # Prerequisite
  target_req <- names(js_def$targets$required)
  target_opt <- names(js_def$targets$optional)
  target_names <- c(target_req, target_opt)

  # - target names (should be the same as in the GitHub)
  if (isFALSE(all(gsub(".+ wk ahead ", "", df$target) %in% target_names))) {
    targetname_test <-  paste0(
      "\U000274c Error 601: At least one of the target_names is misspelled. ",
      "Please verify, the target_names should be (optional target(s) inluded)",
      ": '", paste(target_names, collapse = ", "), "'. The data frame contains",
      ": '", paste(unique(gsub(".+ wk ahead ", "", df$target)),
                   collapse = ", "),
      "', as targets names.")
  } else {
    targetname_test <- NA
  }

  # - the submission contains all the targets. It is also accepted
  # to submit projections for only certain target (for example: only cases, etc.)
  df_req_target <- grep(paste(target_req, collapse = "|"), unique(
    gsub(".+ wk ahead ", "", df$target)), value = TRUE)
  if (length(df_req_target) < 6) {
    targetnum_test <- paste0(
      "\U0001f7e1 Warning 602: The data frame does not contain projections",
      " for '", target_req[!target_req %in% df_req_target],
      "' target(s).")
  }  else {
    targetnum_test <- NA
  }

  # - target_end_date corresponds to the end of the epiweek (Saturday)
  if (isFALSE(all(unique(na.omit(df$target_end_date)) %>%
                  lubridate::wday() %in% 7))) {
    targetday_test <- paste0(
      "\U000274c Error 603: The target_end_date should correspond to the end of ",
      "the epiweek (Saturday). For example, if the 1st week projection is on ",
      "the week starting 2021-11-14 and ending 2021-11-20. The target_end_week",
      " for this week should be: 2021-11-20.")
  } else {
    targetday_test <- NA
  }

  # - target_end_date projects for the expected number of weeks or more
  #  (if a team submit more we will still accept it)
  n_target_week <- length(js_def$horizons$required)
  max_week <- length(unique(c(js_def$horizons$required,
                              js_def$horizons$optional)))
  if (isFALSE(length(unique(na.omit(df$target_end_date))) >= n_target_week)) {
    targetweek_test <- paste0(
      "\U0001f7e1 Warning 605: The projections should contains at least ",
      n_target_week, " weeks of projection. The data frame contains only: ",
      length(unique(df$target_end_date)), " week(s). The projection might ",
      "not be included in the Ensembles.")
  } else {
    if (isTRUE(length(unique(na.omit(df$target_end_date))) > max_week)) {
      targetweek_test <- paste0(
        "\U0001f7e1 Warning 606: The projection contains more projected week ",
        "than expected.")
    } else {
      targetweek_test <- NA
    }}

  # targets information for the horizon
  horizon_target <- purrr::map(c(js_def$targets$required,
                                 js_def$targets$optional), "horizons")
  horizon_target <- purrr::discard(horizon_target, is.null)

  # - all target weeks are present (1,2,3, etc.) by target, scenario, location,
  # quantile for the target(s) requiring it
  hor_target <- names(purrr::keep(purrr::map(horizon_target, "required"),
                                  function(x) x %in% "all"))

  df2 <- dplyr::filter(df, grepl(paste(hor_target, collapse = "|"), target))
  df2 <- dplyr::mutate(
    df2, target_name = gsub(".+ wk ahead ", "", target),
    quantile = ifelse(type == "point", "point", quantile))
  sel_group <- grep(
    "value|target_end_date|type|model_projection_date|scenario_name|target$",
    names(df2), invert = TRUE, value = TRUE)
  lst_df <-  split(df2, as.list(df2[,sel_group]), drop = TRUE)
  targetwnum_test <- lapply(lst_df, function(x) {
    if  (isFALSE(all(
      sort(as.numeric(unique(gsub("[^[:digit:]]", "", x$target)))) ==
      seq_len(length(unique(x$target_end_date))))) |
      (isFALSE(length(unique(
        gsub("[^[:digit:]]", "", x$target))) == n_target_week))) {
      group <- paste(names(unique(x[, sel_group])), ":", unique(x[, sel_group]),
                     collapse = ", ")
      targetwnum_test <- paste0(
        "\U000274c Error 607: At least one target week is missing in the time ",
        "series. Please verify: ", group)
    } else {
      targetwnum_test <- NA
    }
  })

  # - all target weeks are NA by target, scenario, location, quantile/sample
  # for the target(s) requiring it
  na_target <- names(purrr::keep(purrr::map(horizon_target, "required"),
                                 function(x) is.na(x)))
  if (length(na_target) > 0) {
    df2 <- dplyr::filter(df, grepl(paste(na_target, collapse = "|"), target))
    df2 <-  dplyr::mutate(df2, target_name = target,
                          quantile = ifelse(type == "point", "point", quantile))
    sel_group <- grep(
      "value|target_end_date|type|model_projection_date|scenario_name|target$",
      names(df2), invert = TRUE, value = TRUE)
    lst_df <-  split(df2, as.list(df2[,sel_group]), drop = TRUE)
    targetwna_test <- lapply(lst_df, function(x) {
      if (isFALSE(all(is.na(x$target_end_date)))) {
        group <- paste(names(unique(x[, sel_group])), ":", unique(x[, sel_group]),
                       collapse = ", ")
        targetwna_test <- paste0(
          "\U000274c Error 612: The 'target_end_date' should be equal to NA for ",
          "the target. Please verify: ", group)
      } else {
        targetwna_test <- NA
      }
    })
  } else {
    targetwna_test <- NA
  }


  # - start on the good start date
  if (isFALSE(unique(dplyr::filter(df, grepl("^1 wk ahead", target))
                     [, "target_end_date", TRUE]) == as.Date(start_date))) {
    targetstart_test <- paste0(
      "\U000274c Error 608: 1st week target end date is not valid. It should ",
      "be: '", start_date, "' but it is: '", unique(dplyr::filter(
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
      "\U000274c Error 609: One or multiple target(s) end date are not valid. ",
      expected)
  } else {
    targetalldate_test <- NA
  }

  target_test <- na.omit(c(targetname_test,  targetnum_test, targetday_test,
                           targetweek_test, unlist(targetwnum_test),
                           unlist(targetwna_test),
                           targetstart_test, targetalldate_test))
  target_test <- unique(target_test)
  if (length(target_test) == 0)
    target_test <- paste0("No errors or warnings found in target and ",
                          "target_end_date columns")

  return(target_test)
}

