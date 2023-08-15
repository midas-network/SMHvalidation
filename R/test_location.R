#' Runs Validation Checks on the Location column
#'
#' Validate Scenario Modeling Hub submissions: test if the  `location` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param number2location named vector containing the FIPS as name and the
#'  corresponding location name as value (example: name: "01", value: "Alabama")
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
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
test_location <- function(df, number2location, model_task) {
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

  #- targets with specific location does not contains additional location
  loc_test <- lapply(model_task, function(x) {
    # Prerequisite
    req_loc <- x$task_ids$location$required
    opt_loc <- x$task_ids$location$optional
    df_test <- data.table::data.table(df)[target %in% unique(unlist(
      x$task_ids$target)) & output_type %in% names(x$output_type)]
    if (any(nchar(df_test$location) == 1)) {
      df_test$location[which(nchar(df_test$location) == 1)] <- paste0(
        0, df_test$location[which(nchar(df_test$location) == 1)])
    }

    if (dim(df_test)[1] > 0) {
      if (isFALSE(all(is.null(c(req_loc, opt_loc))))) {
        if (all(is.null(req_loc)) & !is.null(opt_loc)) {
          if (isFALSE(all(unique(df_test$location) %in% opt_loc))) {
            loc_test <- paste0(
              "\U000274c Error 703: The submission should only contain",
              " information for the location(s): ",
              paste(opt_loc, collapse = ", "), ", for the target(s): ",
              paste(unique(unlist(x$task_ids$target)), collapse = ", "),
              ". The data frame contains other locations (",
              paste(unique(df_test$location)[!unique(
                df_test$location) %in% opt_loc], collapse = ", "),
              "), please verify.")
          } else {
            loc_test <- NA
          }
        } else {
          if (isFALSE(all(req_loc %in% unique(df_test$location)))) {
            loc_test <- paste0(
              "\U000274c Error 703: The submission should contain information",
              " for the location(s): ",  paste(req_loc, collapse = ", "),
              ", for the target(s): ",
              paste(unique(unlist(x$task_ids$target)), collapse = ", "),
              ". The data frame is missing: ",
              paste(req_loc[!req_loc %in% unique(df_test$location)],
                    collapse = ", "), ", please verify.")
          } else if (isFALSE(all(unique(df_test$location) %in%
                                 c(opt_loc, req_loc)))) {
            if (is.null(opt_loc)) {
              opt_loc_text <- ""
            } else {
                opt_loc_text <- paste0(" and ", paste(opt_loc, collapse = ", "),
                                       " (optional)")
            }
            loc_test <- paste0(
              "\U000274c Error 703: The submission should only contain ",
              "information for the location(s): ", paste(
                req_loc,  collapse = ", "), " (required)", opt_loc_text,
                ", for the target(s): ", paste(unique(unlist(x$target)),
                                               collapse = ", "),
                ". The data frame contains other locations (",
              paste(unique(df_test$location)[!unique(
                df_test$location) %in% c(opt_loc, req_loc)], collapse = ", "),
              "), please verify.")
          } else {
            loc_test <- NA
          }
        }
      } else {
        if (!all(is.na(df_test))) {
          loc_test <-  paste0(
            "\U000274c Error 703: No location should be associated with the ",
            "targets: ", paste(unique(unlist(x$target)), collapse = ", "),
            ". please verify.")
        } else {
          loc_test <- NA
        }
      }
    } else {
      #if (!is.null(x$task_ids$target$required)) {
        #      test_age <-  paste0(
        #        "\U0001f7e1 Warning 513: No value found associated with the ",
        #        "targets: ", paste(unique(unlist(x$task_ids$target)),
        #                           collapse = ", "), ". Please verify.")
      #  loc_test <-  paste0(
      #    "\U0001f7e1 Warning 513: No value found associated with the targets: ",
      #    paste(unique(unlist(x$target)), collapse = ", "),
      #    ". please verify.")
      #}
      loc_test <- NA
    }
    return(loc_test)
  })

  test_loc <- unique(na.omit(c(location_test, unlist(loc_test))))
  if (length(test_loc) == 0)
    test_loc <- "No errors or warnings found on Location"

  return(test_loc)
}
