#' **Deprecated** - Runs Validation Checks on the Location column
#'
#' Validate Scenario Modeling Hub submissions: test if the  `location` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function contains a test:
#' * Specific location: For the target(s) requiring only specific
#'  location(s), no additional location is provided in the submission file
#'
#' Function called in the `validate_submission()` function.
#'
#'@importFrom purrr map
#'@importFrom dplyr filter .data
#'@export
test_location <- function(df, model_task) {

  warning("Function deprecated")
  #- targets with specific location does not contains additional location
  targ_list <- purrr::map(model_task, list("task_ids", "target"))
  req_target <- unique(unlist(purrr::map(targ_list, "required")))
  opt_target <- unique(unlist(purrr::map(targ_list, "optional")))
  lapply(c(req_target, opt_target), function(x) {
    test_task <- model_task[unlist(purrr::map(targ_list,
                                              function(y) any(grepl(x, y))))]
    loc_list <- purrr::map(test_task, list("task_ids", "location"))
    req_loc <- unique(unlist(purrr::map(loc_list, "required")))
    opt_loc <- unique(unlist(purrr::map(loc_list, "optional")))
    outpt_type <- unique(names(unlist(purrr::map(test_task, "output_type"),
                                      FALSE)))
    df_test <- dplyr::filter(df, .data[["target"]] %in% x &
                               .data[["output_type"]] %in% outpt_type)
    df_test <- loc_zero(df_test)

    if (dim(df_test)[1] > 0) {
      # If at least one location expected
      if (isFALSE(all(is.null(c(req_loc, opt_loc))))) {
        # If all location optional
        if (all(is.null(req_loc) & !is.null(opt_loc))) {
          if (isFALSE(all(unique(df_test$location) %in% opt_loc))) {
            loc_mess <-
              paste(unique(df_test$location)[!unique(df_test$location) %in%
                                               opt_loc], collapse = ", ")
            message("\U000274c Error: The submission should ",
                    "only contain information for the location(s): ",
                    paste(opt_loc, collapse = ", "), ", for the target: ",
                    x, ". The data frame contains other locations (", loc_mess,
                    "), please verify.")
          }
          # If not all location optional
        } else {
          # if all required location contains in the data frame
          if (isFALSE(all(req_loc %in% unique(df_test$location)))) {
            message("\U000274c Error: The submission should contain ",
                    "information for the location(s): ",
                    paste(req_loc, collapse = ", "), ", for the target: ", x,
                    ". The data frame is missing: ",
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
            message("\U000274c Error: The submission should only contain ",
                    "information for the location(s): ",
                    paste(req_loc,  collapse = ", "), " (required)",
                    opt_loc_text, ", for the target: ", x,
                    ". The data frame contains other locations (", loc_mess,
                    "), please verify.")
          }
        }
      } else {
        if (!all(is.na(df_test$location))) {
          message("\U000274c Error: No location should be associated with",
                  " the target: ", x, ". please verify.")
        }
      }
    }
    invisible(NULL)
  })
  invisible(NULL)
}
