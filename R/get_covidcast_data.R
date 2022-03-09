#' Fix negative case counts
#'
#' Fix negative case counts
#'
#' @param .x data frame
#' @param .y data frame
#' @param incid_col_name character vector
#' @param date_col_name character vector
#' @param cum_col_name character vector
#' @param type character vector: must be one of ['low', 'med', 'high']
#'
#' @details There are three different ways of dealing with negative incidence
#' counts, specified by the type argument:
#' \itemize{
#'  \item{"high" method: }{Highest estimate; this modifies the cumulative column
#'   to be the cummax of the previous cumulative counts.}
#'  \item{"low" method: }{Assume all negative incidents are corrections to
#'  previous reports. So, reduce previous reports to make incidents not
#'  decrease.}
#'  \item{"mid" method: }{Combination of both low and high.}
#' }
#' For consecutive negatives, the first negative will be modified so that the
#'   cumulative is the cummax so far.
#' For the following negatives, use the low method: reduce previous reports.
#' For get_USAFacts_data(), this is always the default ("mid"), but the other
#'  code is here for posterity.
#'
#' @keywords internal
#' @importFrom dplyr arrange lag
#' @importFrom rlang sym !!
#' @noRd
fix_negative_counts_single_geoid <- function(.x, .y, incid_col_name,
                                             date_col_name, cum_col_name,
                                             type) {
  original_names <- names(.x)

  .x <- dplyr::arrange(.x,!!rlang::sym(date_col_name))

  # Calculate new cumulative column
  calc_col_name <- paste(cum_col_name, type, sep="_")

  if (type == "high") {

    .x[[calc_col_name]] <- .x[[cum_col_name]]
    .x[[calc_col_name]][is.na(.x[[calc_col_name]]) ] <- 0 # Fixes NA values
    .x[[calc_col_name]] <- cummax(.x[[calc_col_name]])

  } else if (type == "low") {

    .x[[calc_col_name]] <- .x[[cum_col_name]]
    .x[[calc_col_name]][is.na(.x[[calc_col_name]])] <- Inf # Fixes NA values
    .x[[calc_col_name]] <- rev(cummin(rev(.x[[calc_col_name]])))
    # In case last thing in row is NA/infinity
    .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- 0
    .x[[calc_col_name]] <- cummax(.x[[calc_col_name]]) # Replace 0's with cummax

  } else if (type == "mid") {

    .x[[calc_col_name]] <- .x[[cum_col_name]]

    ## In mid, do a local max followed by a global min (like "low")

    # Get indices where the cumulative count is bigger than the one after it
    # i.e. where incidence is negative next time
    unlagged <- .x[[calc_col_name]]
    unlagged[is.na(.x[[calc_col_name]]) ] <- -Inf # Fixes NA values
    lagged <- dplyr::lag(.x[[calc_col_name]])
    lagged[is.na(.x[[calc_col_name]]) ] <- Inf # Fixes NA values

    max_indices <- which(unlagged < lagged)
    .x[[calc_col_name]][max_indices] <- lagged[max_indices]

    # Global min
    .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- Inf
    .x[[calc_col_name]] <- rev(cummin(rev(.x[[calc_col_name]])))
    .x[[calc_col_name]][!is.finite(.x[[calc_col_name]]) ] <- 0
    .x[[calc_col_name]] <- cummax(.x[[calc_col_name]])

  } else {

    stop(paste(
      "Invalid fix_negative_counts type. Must be ['low', 'med', 'high'].",
      "Actual:", type))

  }

  .x[[cum_col_name]] <- .x[[calc_col_name]]

  # Recover incidence from the new cumulative
  .x[[incid_col_name]] <- c(.x[[calc_col_name]][[1]], diff(.x[[calc_col_name]]))

  .x <- .x[,original_names]
  for(col in names(.y)){
    .x[[col]] <- .y[[col]]
  }

  return(.x)
}

#' Add missing dates, fix counts that go negative, and fix NA values
#'
#' See fix_negative_counts_single_geoid() or the details section for more
#' details on the algorithm, specified by argument "type"
#'
#' @param df data frame
#' @param cum_col_name character vector
#' @param incid_col_name character vector
#' @param date_col_name character vector, by default "Update"
#' @param min_date Date, by default `min(df[[date_col_name]])`
#' @param max_date Date, by default `max(df[[date_col_name]])`
#' @param type character vector: must be one of ['low', 'med', 'high']. By
#' default `"mid"`
#'
#'#' @details There are three different ways of dealing with negative incidence
#' counts, specified by the type argument:
#' \itemize{
#'  \item{"high" method: }{Highest estimate; this modifies the cumulative column
#'   to be the cummax of the previous cumulative counts.}
#'  \item{"low" method: }{Assume all negative incidents are corrections to
#'  previous reports. So, reduce previous reports to make incidents not
#'  decrease.}
#'  \item{"mid" method: }{Combination of both low and high.}
#' }
#' For consecutive negatives, the first negative will be modified so that the
#'   cumulative is the cummax so far.
#' For the following negatives, use the low method: reduce previous reports.
#' For get_USAFacts_data(), this is always the default ("mid"), but the other
#'  code is here for posterity.
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr filter group_by group_map bind_rows anti_join
#' @importFrom data.table data.table
#'
fix_negative_counts <- function(
  df,
  cum_col_name,
  incid_col_name,
  date_col_name = "Update",
  min_date = min(df[[date_col_name]]),
  max_date = max(df[[date_col_name]]),
  type="mid" # "low" or "high"
) {

  if(nrow(dplyr::filter(df, !!incid_col_name < 0)) > 0) {
    return(df)
  }

  df <- dplyr::group_by(df, FIPS,source)
  # Add missing dates
  df_date <- lapply(unique(df$source), function(x) {
    data.table::data.table(
      source = x,
      FIPS = unique(df[df$source == x, ][["FIPS"]]),
      date = (min_date + seq_len(max_date - min_date) - 1))
  }) %>% dplyr::bind_rows()
  colnames(df_date) <- gsub("^date$", date_col_name, colnames(df_date))
  df_date <- dplyr::anti_join(df_date, df)
  missing_col <- grep(paste(colnames(df_date), collapse = "|"),
                      colnames(df), value = TRUE, invert = TRUE)
  df_missing_col <- t(data.table::data.table(rep(NA, length(missing_col))))
  colnames(df_missing_col) <- missing_col
  df_missing_col <- data.table::data.table(df_missing_col)
  df_date <- cbind(df_date, df_missing_col)

  df <- rbind(df, df_date)

  #df <- tidyr::complete(df, !!rlang::sym(date_col_name) := min_date +
  #                          seq_len(max_date - min_date) - 1)
  df <- dplyr::group_map(df, fix_negative_counts_single_geoid,
                         incid_col_name = incid_col_name,
                         date_col_name = date_col_name,
                         cum_col_name = cum_col_name,
                         type = type)
  df <- do.call(what = rbind, df)

  return(df)
}

#' Get "covidcast" data
#'
#' Download and returns data from COVIDcast package, with daily data available
#' at multiple geographical level.
#'
#' @param geo_level character, geographical level ("state" or "county").
#' By default, `"state"`
#' @param signals character, one or multiple signals accepted by the covidcast
#' package. By defaults: `c("deaths_incidence_num", "deaths_cumulative_num",
#'  "confirmed_incidence_num", "confirmed_cumulative_num",
#'  "confirmed_admissions_covid_1d")`
#' @param limit_date Date, by default, `Sys.Date()`
#' @param fix_negatives Boolean, by default TRUE. For more information, please
#' look at the documentation of the function `?fix_negative_counts()`
#' @param run_parallel Boolean, by default FALSE. If TRUE, will run the call
#' to the covidcast package in parallel.
#' @param n_cores numeric, by default 4. Number of cores to use when
#' run_parallel is set to TRUE
#'
#' @details For `covidcast` R package. Please, look at
#'\url{https://cran.r-project.org/web/packages/covidcast/index.html} for more
#'information. \cr\cr
#'
#' @return a data frame
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter mutate select full_join %>% left_join rename arrange
#' @importFrom dplyr group_by ungroup distinct as_tibble
#' @importFrom stats setNames
#' @importFrom lubridate year as_date
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %dopar% %do% foreach
#' @importFrom covidcast covidcast_signal
#' @importFrom data.table rbindlist
#' @importFrom stringr str_length str_replace fixed
#' @importFrom tidyr replace_na pivot_wider
#'
#' @examples
#' df <- get_covidcast_data()
#'
get_covidcast_data <- function(
  geo_level = "state",
  signals = c("deaths_incidence_num", "deaths_cumulative_num",
              "confirmed_incidence_num", "confirmed_cumulative_num",
              "confirmed_admissions_covid_1d"),
  limit_date = Sys.Date(),
  fix_negatives = TRUE,
  run_parallel = FALSE,
  n_cores = 4){

  # Create dictionary
  # From the GitHub: https://github.com/reichlab/covid19-forecast-hub
  loc_dictionary <- readr::read_csv(
    "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv")
  loc_abbr <- loc_dictionary %>% dplyr::filter(!is.na(abbreviation))
  loc_dictionary <- loc_dictionary %>%
    dplyr::mutate(state_fips = substr(location, 1, 2)) %>%
    dplyr::select(-abbreviation) %>%
    dplyr::full_join(loc_abbr %>%
                       dplyr::select(abbreviation, state_fips = location))

  # in folder data-locations
  loc_dictionary_name <- suppressWarnings(
    setNames(c(rep(loc_dictionary$location_name, 2), "US",
               rep(loc_dictionary$location_name[-1], 2),
               rep(loc_dictionary$location_name, 2),
               "New York"),
             c(loc_dictionary$location,
               tolower(loc_dictionary$abbreviation), "US",
               na.omit(as.numeric(loc_dictionary$location)),
               as.character(na.omit(
                 as.numeric(loc_dictionary$location))),
               tolower(loc_dictionary$location_name),
               toupper(loc_dictionary$location_name),
               "new york state")))

  loc_dictionary_abbr <- setNames(loc_dictionary$abbreviation,
                                  loc_dictionary$location)
  loc_dictionary_pop <- setNames(loc_dictionary$population,
                                 loc_dictionary$location)

  # Set up start and end dates of data to pull
  # -- we pull the data in 6-month chunks to speed up and not overwhelm API
  #    for county-level

  if (geo_level=="county"){
    years_ <- lubridate::year("2020-01-01"):lubridate::year(limit_date)
    start_dates <- sort(c(lubridate::as_date(paste0(years_, "-01-01")),
                          lubridate::as_date(paste0(years_, "-07-01"))))
    start_dates <- start_dates[start_dates<=limit_date]

    end_dates <- sort(c(lubridate::as_date(paste0(years_, "-06-30")),
                        lubridate::as_date(paste0(years_, "-12-31")),
                        limit_date))
    end_dates <- end_dates[end_dates <= limit_date]
  } else {
    start_dates <- lubridate::as_date("2020-01-01")
    end_dates <- lubridate::as_date(limit_date)
  }

  # Set up parallelization to speed up
  if (run_parallel){
    doParallel::registerDoParallel(cores=n_cores)
    `%do_fun%` <- foreach::`%dopar%`
  } else {
    `%do_fun%` <- foreach::`%do%`
  }

  res <- foreach::foreach(
    x = signals,
    .combine = rbind,
    .packages = c("covidcast","dplyr","lubridate", "doParallel","foreach",
                  "vroom","purrr"),
    .verbose = TRUE) %do_fun% {
      start_dates_ <- start_dates
      if (x == "confirmed_admissions_covid_1d"){
        start_dates_[1] <- lubridate::as_date("2020-02-01")
      }
      # Call API to generate gold standard data from COVIDCast
      df <- lapply(1:length(start_dates),
                   FUN = function(y = x) {
                     covidcast::covidcast_signal(
                       data_source = ifelse(grepl("admissions", x), "hhs",
                                            "jhu-csse"),
                       signal = x, geo_type = geo_level,
                       start_day = lubridate::as_date(start_dates_[y]),
                       end_day = lubridate::as_date(end_dates[y]))})
      df <- data.table::rbindlist(df)
      if (geo_level=="state") {
        df <- df %>% dplyr::mutate(state_abbr = toupper(geo_value)) %>%
          dplyr::select(-geo_value) %>%
          dplyr::left_join(loc_dictionary %>%
                             dplyr::select(state_abbr = abbreviation,
                                           geo_value = location) %>%
                             dplyr::filter(stringr::str_length(geo_value)==2))
      }
      df <- df %>% dplyr::rename(date = time_value)

      # Get cum hospitalizations
      if (x == "confirmed_admissions_covid_1d") {
        df_cum <- df %>%
          dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
          dplyr::arrange(state_abbr, geo_value, date) %>%
          dplyr::group_by(data_source, signal, geo_value, state_abbr) %>%
          dplyr::mutate(value = cumsum(value)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(signal = "confirmed_admissions_cum")
        df <- rbind(df, df_cum)
      }
      df %>% dplyr::select(signal, Update = date, source = state_abbr,
                           FIPS = geo_value, value)
      }
  res <- res %>%
    dplyr::mutate(
      signal = dplyr::recode(
        signal,
        "deaths_incidence_num" = "incidDeath",
        "deaths_cumulative_num" = "Deaths",
        "confirmed_incidence_num" = "incidI",
        "confirmed_cumulative_num" = "Confirmed",
        "confirmed_admissions_covid_1d" = "incidH",
        "confirmed_admissions_cum" = "Hospitalizations")) %>%
    tidyr::pivot_wider(names_from = signal, values_from = value) %>%
    dplyr::mutate(Update = lubridate::as_date(Update),
                  # clean FIPS if numeric
                  FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), ""),
                  FIPS = paste0(FIPS, "000")) %>%
    dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>%
    dplyr::distinct()

  validation_date <- Sys.getenv("VALIDATION_DATE")
  if ( validation_date != '' ) {
    print(paste("(DataUtils.R) Limiting CSSE US data to:", validation_date,
                sep=" "))
    res <- dplyr::filter(res, Update < validation_date)
  }

  res <- res %>% as_tibble()

  # Fix incidence counts that go negative and NA values or missing dates
  if (fix_negatives & any(c("Confirmed", "incidI", "Deaths", "incidDeath") %in%
                          colnames(res))){
    res <- fix_negative_counts(res, "Confirmed", "incidI") %>%
      fix_negative_counts("Deaths", "incidDeath")
  }
  return(res)
}

