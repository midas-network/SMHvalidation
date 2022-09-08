#' Runs Validation Checks on the Location column
#'
#' Validate Scenario Modeling Hub submissions: test if the  `location` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param number2location named vector containing the FIPS as name and the
#'  corresponding location name as value (example: name: "01", value: "Alabama")
#'@param js_def list containing round definitions: number and names of columns,
#' target names, ...
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
test_location <- function(df, number2location, js_def) {
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
  target_loc <- purrr::map(c(js_def$targets$required, js_def$targets$optional),
                           "location")
  target_loc <- purrr::discard(target_loc, is.null)
  target_loc <- purrr::keep(purrr::map(target_loc, "required"),
                            function(x) x != "all")
  loc_target_name <- names(target_loc)
  targ_loc_test <- lapply(loc_target_name, function(x) {
    test <- target_loc[[x]]
    df_test <- dplyr::filter(df, !grepl(paste(test, collapse = "|"), location),
                             grepl(x, target))
    if (dim(df_test)[1] > 0) {
      loc_test <- paste0("\U000274c Error 703: ",
        "The target ", x, " should only contain the location(s):",
        paste(test, collapse = ", "), ", the data frame contains other ",
        "locations (", paste(unique(df_test$location), collapse = ", "),
        "), please verify.")
    } else {
      loc_test <- NA
    }
    return(loc_test)
  })

  test_loc <- unique(na.omit(c(location_test, unlist(targ_loc_test))))
  if (length(test_loc) == 0)
    test_loc <- "No errors or warnings found on Location"

  return(test_loc)
}
