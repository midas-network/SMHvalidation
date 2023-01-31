#' Runs Validation Checks on the Location column
#'
#' Validate Scenario Modeling Hub submissions: test if the  `location` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param number2location named vector containing the FIPS as name and the
#'  corresponding location name as value (example: name: "01", value: "Alabama")
#'@param task_ids data.frame containing round information for each id columns
#'
#'@details  This function contains 2 tests:
#'\itemize{
#'  \item{Location name: }{The submission should contains projection by location,
#'  the `location` column contains the location FIPS number as available in the
#'  location table in the SMH GitHub Repository. If the FIPS number are missing
#'  a trailing zero, the submission will be accepted but a warning message
#'  will be returned.}
#'  \item{Specific location: }{For the target(s) requiring only specific
#'  location(s), no additional location is provided in the submission file}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom purrr keep map discard
#'@importFrom dplyr filter
#'@export
test_location <- function(df, number2location, task_ids) {
  # - correspond to the table in the GitHub
  if (isFALSE(!any(is.na(number2location[unique(df$location)])))) {
    vect <- vect0 <- df$location
    if (any(nchar(vect) == 1)) {
      vect0 <- vect
      vect[which(nchar(vect) == 1)] <- paste0(0,  vect[which(nchar(vect) == 1)])
    }
    if (isFALSE(!any(is.na(number2location[unique(vect)])))) {
      location_test <- paste0(
        "\U000274c Error 701: Some locations codes are not corresponding to ",
        "any known location: '", paste(unique(df$location)[is.na(
          number2location[unique(df$location)])], collapse = "', '"), "'.")
    } else {
      # is missing the trailing 0
      location_test <- paste0(
        "\U0001f7e1 Warning 702: Some location value are missing a trailing 0.",
        " For example, ", vect0[which(nchar(vect0) == 1)], " instead of ",
        paste0(0,  vect0[which(nchar(vect0) == 1)]))
    }
  } else {
    location_test <- NA
  }

  #- targets woth specific location does not contains additional location
  req_loc <- task_ids$location$required[[1]]
  opt_loc <- task_ids$location$optional[[1]]
  if (isFALSE(all(is.na(c(req_loc, opt_loc))))) {
    if (all(is.na(req_loc))) {
      if (isFALSE(all(unique(df$location) %in% opt_loc))) {
        loc_test <- paste0(
          "\U000274c Error 703: The submission should only contain information",
          " for the location(s): ",  paste(opt_loc, collapse = ", "),
          ", the data frame contains other locations (",
          paste(unique(df$location)[!unique(df$location) %in% opt_loc],
                collapse = ", "), "), please verify.")
      } else {
        loc_test <- NA
      }
    } else {
      if (isFALSE(all(req_loc %in% unique(df$location)))) {
        loc_test <- paste0(
          "\U000274c Error 703: The submission should only contain information",
          " for the location(s): ",  paste(req_loc, collapse = ", "),
          ", the data frame contains other locations (",
          paste(req_loc[!req_loc %in% unique(df$location)],
                collapse = ", "), "), please verify.")
      } else if (isFALSE(all(unique(df$location) %in% c(opt_loc, req_loc)))) {
        loc_test <- paste0(
          "\U000274c Error 703: The submission should only contain information",
          " for the location(s): ",  paste(req_loc, collapse = ", "),
          " (required) and ",  paste(opt_loc, collapse = ", "), " (optional)",
          ", the data frame contains other locations (",
          paste(unique(df$location)[!unique(df$location) %in%
                                      c(opt_loc, req_loc)], collapse = ", "),
          "), please verify.")
      } else {
        loc_test <- NA
      }
    }
  } else {
    loc_test <- NA
  }

  test_loc <- unique(na.omit(c(location_test, loc_test)))
  if (length(test_loc) == 0)
    test_loc <- "No errors or warnings found on Location"

  return(test_loc)
}
