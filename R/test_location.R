#' Runs Validation Checks on the Location column
#'
#' Validate Scenario Modeling Hub submissions: test if the  `location` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param number2location named vector containing the FIPS as name and the
#'  corresponding location name as value (example: name: "01", value: "Alabama")
#'
#'@details  This functions contains 1 test:
#'\itemize{
#'  \item{Location name: }{The submission should contains projection by location,
#'  the `location` column contains the location FIPS number as available in the
#'  location table in the SMH GitHub Repository. If the FIPS number are missing
#'  a trailing zero, the submission will be accepted but a warning message
#'  will be returned.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@export
test_location <- function(df, number2location) {
  # - correspond to the table in the GitHub
  if (isFALSE(!any(is.na(number2location[unique(df$location)])))) {
    vect <- vect0 <- df$location
    if (any(nchar(vect) == 1)) {
      vect0 <- vect
      vect[which(nchar(vect) == 1)] <- paste0(0,  vect[which(nchar(vect) == 1)])
    }
    if (isFALSE(!any(is.na(number2location[unique(vect)])))) {
      location_test <- paste0(
        "\U000274c Error: Some locations codes are not corresponding to any ",
        "location: '", paste(unique(df$location)[is.na(number2location[unique(
          df$location)])], collapse = ", "), "'.")
    } else {
      # is missing the trailing 0
      location_test <- paste0(
        "\U0001f7e1 Warning: Some location value are missing a trailing 0. ",
        "For example, ", vect0[which(nchar(vect0) == 1)], " instead of ",
        paste0(0,  vect0[which(nchar(vect0) == 1)]))
    }
  } else {
    location_test <- NA
  }

  test_loc <- na.omit(unique(location_test))
  if (length(test_loc) == 0)
    test_loc <- "No errors or warnings found on Location"

  return(test_loc)
}
