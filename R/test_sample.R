#' Runs Validation Checks on the sample column
#'
#' Validate Scenario Modeling Hub submissions: test if the  `sample` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'@param pairing_col column names indicating the sample pairing information. By
#' default: "horizon".
#'
#'@details  This function contains 3 tests:
#'\itemize{
#'  \item{sample number: }{The submission should contain a sample column with
#'  the expected number of trajectory for each
#'  scenario/target/location/horizon (age_group) group}
#'  \item{unique sample: }{The submission should at least contain a unique
#'  sample identifier by pairing group, for example if pairing_col = "horizon",
#'  the sample identifier `1` should contain all the possible horizon value,
#'  and optionally can contain the specific and multiple value for the other
#'  task id column}
#'  \item{sample integer: }{The submission should contain a sample column with
#'  integer only.}
#'  \item{required sample:}{If a target expected sample type output, the
#'  submission should contain sample type output for this target}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter n
#'@export
test_sample <- function(df, model_task, pairing_col = "horizon") {

  test_sample <- lapply(model_task, function(x) {
    if ("sample" %in% names(x$output_type)) {
      # prerequisite
      df_sample <- data.table::data.table(df) %>%
        dplyr::mutate(origin_date = as.Date(origin_date)) %>%
        loc_zero()
      tasks_list <- setNames(lapply(names(x$task_ids),
                                    function(z) unique(unlist(x$task_id[[z]]))),
                             names(x$task_ids))
      test_df <- expand.grid(tasks_list) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate(origin_date = as.Date(origin_date))
      test_df$sel <- 1
      df_sample <- dplyr::left_join(df_sample, test_df,
                                    by = names(tasks_list)) %>%
        dplyr::filter(sel == 1, output_type == "sample") %>%
        dplyr::select(-sel)
      vector_sample <- unlist(unique(df_sample[, output_type_id]))
      task_ids <- x$task_ids
      # - sample column should be an integer
      if (dim(df_sample)[1] > 0) {
        if (isFALSE(all(is_wholenumber(vector_sample)))) {
          sample_type <-
            paste0("\U000274c Error 903: The column 'output_type_id' should ",
                   "contains integer values only for type 'sample'. ",
                   "Please verify")
        } else {
          sample_type <- NA
        }

        df_sample$output_type_id <- as.integer(df_sample$output_type_id)
        vector_sample <- as.integer(vector_sample)
        # - sample output_type_id column contains the expected value
        out_type_id <- x$output_type$sample$output_type_id
        if ("required" %in% names(out_type_id)) {
          exp_sample <- as.numeric(unique(c(out_type_id$required,
                                            out_type_id$optional)))
          req_sample_max <- out_type_id$required
          if (is.null(req_sample_max))
            req_sample_max <- out_type_id$optional
        } else {
          exp_sample <- out_type_id$max_samples_per_task
          req_sample_max <- out_type_id$min_samples_per_task
        }

        test_sample <- dplyr::group_by(df_sample,
                                       dplyr::across(names(task_ids)))
        test_sample <- summarise(test_sample, n = dplyr::n())
        n_sample <- unique(test_sample$n)

        if (length(n_sample) > 1) {
          sample_value <-
            paste0("\U000274c Error 904: All the groups should contains the ",
                   "same number of trajectories per group. Please verify.")
        } else if (n_sample != max(exp_sample)) {
          if (n_sample < max(as.numeric(req_sample_max))) {
            sample_value <-
              paste0("\U0001f7e1 Warning 901: The column 'output_type_id' ",
                     "should contains at least ",
                     max(as.numeric(req_sample_max)),
                     " (included) trajectories per group for the type 'sample'",
                     ", the submission file contains: ",
                     paste(n_sample, collapse = ", "),
                     " trajectories per group. Please verify.")
          } else if (n_sample > max(exp_sample)) {
            sample_value <-
              paste0("\U0001f7e1 Warning 901: The column 'output_type_id' ",
                     "should contains a maximum of ", max(exp_sample),
                     " (included) trajectories per group for the type 'sample'",
                     ", the submission file contains: ",
                     paste(n_sample, collapse = ", "),
                     " trajectories per group. Please verify.")
          } else {
            sample_value <- NA
          }
        } else {
          sample_value <- NA
        }

        # - sample id should contains the expected number of repetition
        # for each group (task_ids)
        # For pairing information: test if only minimum pairing repeated the
        # expected number of times
        in_list <- function(df, list) {
          bool_list <- lapply(seq_along(list), function(x) {
            bool <- all(list[[x]] %in% df[[names(list[x])]])
            return(bool)
          })
          all(unlist(bool_list))
        }
        sample_group <- df_sample %>%
          dplyr::select(output_type_id, dplyr::all_of(pairing_col)) %>%
          dplyr::distinct()
        exp_list <- setNames(lapply(c(pairing_col),
                                    function(x) unique(df_sample[[x]])),
                             c(pairing_col))
        sample_group_test <- sample_group %>%
          split(sample_group$output_type_id) %>%
          purrr::map(in_list, exp_list) %>%
          unlist()
        if (any(!sample_group_test)) {
          sample_unique <-
            paste0("\U000274c Error 902: The minimal accepted grouping ",
                   "includes the column(s): ",
                   paste(pairing_col, collapse = ", "), ", please  verify.")
        } else {
          sample_unique <- NA
        }
        # - result output
        test_sample <- unique(na.omit(c(sample_value, unlist(sample_unique),
                                        sample_type)))
      } else {
        if (!is.null(x$task_ids$target$required) &
              ("sample" %in% names(x$output_type))) {
          if ("required" %in% names(x$output_type$sample$output_type_id)) {
            if (!is.null(x$output_type$sample$output_type_id$required)) {
              test_sample <-
                paste0("\U000274c Error 904: Samples are expected in the ",
                       "submission for the target(s): ",
                       paste(unique(unlist(x$task_ids$target$required)),
                             collapse = ", "), ". please verify.")
            } else {
              test_sample <- NA
            }
          } else {
            test_sample <-
              paste0("\U000274c Error 904: Samples are expected in the ",
                     "submission for the target(s): ",
                     paste(unique(unlist(x$task_ids$target$required)),
                           collapse = ", "), ". please verify.")
          }
        } else {
          test_sample <-  NA
        }
      }
    } else {
      test_sample <- NA
    }
    return(test_sample)
  })

  test_sample <- unique(na.omit(unlist(test_sample)))
  if (length(test_sample) == 0)
    test_sample <- "No errors or warnings found on Sample"
  return(test_sample)
}
