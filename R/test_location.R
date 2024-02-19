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
#'  \item{Location name: }{The submission should contains projection by
#'  location, the `location` column contains the location FIPS number as
#'  available in the location table in the SMH GitHub Repository. If the FIPS
#'  number are missing a trailing zero, the submission will be accepted but a
#'  warning message will be returned.}
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
      loc_mess <- paste(unique(df$location)
                        [is.na(number2location[unique(df$location)])],
                        collapse = "', '")
      location_test <-
        paste0("\U000274c Error 701: Some locations codes are not ",
               "corresponding to any known location: '", loc_mess, "'.")
    } else {
      # is missing the trailing 0
      location_test <- paste0("\U0001f7e1 Warning 702: Some location value are",
                              " missing a trailing 0. For example, ",
                              vect0[which(nchar(vect0) == 1)], " instead of ",
                              paste0(0,  vect0[which(nchar(vect0) == 1)]))
    }
  } else {
    location_test <- NA
  }

  #- targets with specific location does not contains additional location
  targ_list <- purrr::map(model_task, list("task_ids", "target"))
  req_target <- unique(unlist(purrr::map(targ_list, "required")))
  opt_target <- unique(unlist(purrr::map(targ_list, "optional")))
  loc_test <- lapply(c(req_target, opt_target), function(x) {
    test_task <- model_task[unlist(purrr::map(targ_list,
                                              function(y) any(grepl(x, y))))]
    loc_list <- purrr::map(test_task, list("task_ids", "location"))
    req_loc <- unique(unlist(purrr::map(loc_list, "required")))
    opt_loc <- unique(unlist(purrr::map(loc_list, "optional")))
    outpt_type <- unique(names(unlist(purrr::map(test_task, "output_type"),
                                      FALSE)))
    df_test <- data.table::data.table(df)[(target %in% x) &
                                            (output_type %in% outpt_type)]
    if (any(nchar(df_test$location) == 1)) {
      df_test$location[which(nchar(df_test$location) == 1)] <-
        paste0(0, df_test$location[which(nchar(df_test$location) == 1)])
    }

    if (dim(df_test)[1] > 0) {
      # If at least one location expected
      if (isFALSE(all(is.null(c(req_loc, opt_loc))))) {
        # If all location optional
        if (all(is.null(req_loc) & !is.null(opt_loc))) {
          if (isFALSE(all(unique(df_test$location) %in% opt_loc))) {
            loc_mess <-
              paste(unique(df_test$location)[!unique(df_test$location) %in%
                                               opt_loc], collapse = ", ")
            loc_test <-
              paste0("\U000274c Error 703: The submission should ",
                     "only contain information for the location(s): ",
                     paste(opt_loc, collapse = ", "), ", for the target: ",
                     x, ". The data frame contains other locations (", loc_mess,
                     "), please verify.")
          } else {
            loc_test <- NA
          }
          # If not all location optional
        } else {
          # if all required location contains in the data frame
          if (isFALSE(all(req_loc %in% unique(df_test$location)))) {
            loc_test <-
              paste0("\U000274c Error 703: The submission should contain ",
                     "information for the location(s): ",
                     paste(req_loc, collapse = ", "), ", for the target: ",
                     x, ". The data frame is missing: ",
                     paste(req_loc[!req_loc %in% unique(df_test$location)],
                           collapse = ", "), ", please verify.")
            # if all location in optional and required
          } else if (isFALSE(all(unique(df_test$location) %in%
                                   c(opt_loc, req_loc)))) {
            if (is.null(opt_loc)) {
              opt_loc_text <- ""
            } else { # nocov start
              opt_loc_text <- paste0(" and ", paste(opt_loc, collapse = ", "),
                                     " (optional)")
            } # nocov end
            loc_mess <-
              paste(unique(df_test$location)[!unique(df_test$location) %in%
                                               c(opt_loc, req_loc)],
                    collapse = ", ")
            loc_test <-
              paste0("\U000274c Error 703: The submission should only contain ",
                     "information for the location(s): ",
                     paste(req_loc,  collapse = ", "), " (required)",
                     opt_loc_text, ", for the target: ", x,
                     ". The data frame contains other locations (", loc_mess,
                     "), please verify.")
          } else {
            loc_test <- NA
          }
        }
      } else {
        if (!all(is.na(df_test$location))) {
          loc_test <-
            paste0("\U000274c Error 703: No location should be associated with",
                   " the target: ", x, ". please verify.")
        } else {
          loc_test <- NA
        }
      }
    } else {
      loc_test <- NA
    }
    return(loc_test)
  })

  test_loc <- unique(na.omit(c(location_test, unlist(loc_test))))
  if (length(test_loc) == 0)
    test_loc <- "No errors or warnings found on Location"

  return(test_loc)
}
