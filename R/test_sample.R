verbose_pairing <- function(df_sample, m_task, or_pair, n_sample,
                            verbose_col = NULL) {
  if (length(unique(df_sample$run_grouping)) > 1) {
    run_group <- purrr::map(dplyr::group_split(df_sample, run_grouping),
                            paired_info,
                            rm_col = c("stochastic_run",
                                       "output_type_id",
                                       "output_type", "value"),
                            tasks_list = m_task$task_ids,
                            verbose_col = verbose_col) %>%
      unique() %>%
      purrr::map(c, or_pair)
  } else {
    run_group <- "No run grouping"
  }
  if (length(unique(df_sample$stochastic_run)) > 1) {
    sto_group <- purrr::map(dplyr::group_split(df_sample,
                                               stochastic_run),
                            paired_info, c("run_grouping",
                                           "output_type_id",
                                           "output_type", "value"),
                            tasks_list = m_task$task_ids,
                            verbose_col = verbose_col) %>%
      unique() %>%
      purrr::map(c, or_pair)
  } else {
    sto_group <- "No stochasticity"
  }
  head_mess <- "\n ### Column Pairing information: \n "
  p_rg <- paste0(head_mess, "Run grouping pairing: \n",
                 paste(run_group, collapse = ", \n"))
  p_info <- paste0(p_rg, "\n",
                   paste0(" Stochastic run pairing: \n",
                          paste(sto_group, collapse = ", \n")))
  pair_inf <- paste0(p_info, "\n ",
                     "Number of Samples: ", n_sample)
  return(pair_inf)
}


pairing_test <- function(df_sample, m_task, or_pair, pairing_col) {
  if (length(unique(df_sample$run_grouping)) > 1) {
    run_group <- purrr::map(dplyr::group_split(df_sample, run_grouping),
                            paired_info,
                            rm_col = c("stochastic_run", "output_type_id",
                                       "output_type", "value"),
                            tasks_list = m_task$task_ids) %>%
      unique() %>%
      purrr::map(c, or_pair)
  } else {
    run_group <- NULL
  }
  if (length(unique(df_sample$stochastic_run)) > 1) {
    sto_group <- purrr::map(dplyr::group_split(df_sample, stochastic_run),
                            paired_info, c("run_grouping",
                                           "output_type_id",
                                           "output_type", "value"),
                            tasks_list = m_task$task_ids) %>%
      unique() %>%
      purrr::map(c, or_pair)
  } else {
    sto_group <- NULL
  }
  sub_pair <- purrr::compact(c(sto_group, run_group))
  miss_col <- purrr::map(sub_pair,
                         function(l) pairing_col[!(pairing_col %in% l)])
  miss_col <- unique(unlist(miss_col))
  if (length(miss_col) > 0) {
    sample_unique <-
      paste0("\U000274c Error 902: The minimal accepted grouping ",
             "includes the column(s): ",
             paste(pairing_col, collapse = ", "), ", the column(s): ",
             paste(miss_col, collapse = ", "), " seem to be missing (or ",
             "is missing a sub-group), please  verify.")
  } else {
    sample_unique <- NA
  }
  return(sample_unique)
}

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
#'@param verbose Boolean, if TRUE add information about the sample pairing
#'  information in output message
#'@param verbose_col character vector, name of columns to print value samples
#' are paired on.
#'
#'@details  This function contains 3 tests:
#' * sample number: The submission should contain the expected number of
#'  trajectory for each model tasks group (unique combination of `task_ids`
#'  columns as specified in the `model_task` parameter).
#' * unique sample: The submission should at least contain a unique
#'  sample identifier by pairing group, for example if pairing_col = "horizon",
#'  the sample identifier `1` should contain all the possible horizon value,
#'  and optionally can contain the specific and multiple value for the other
#'  task id column.
#' * sample integer: The submission should contain a `output_type_id`
#'  column with integer only, associated with `"sample"` output type.
#' * required sample: If a target expected `"sample"` type output, the
#'  submission should contain `"sample"` type output for this target.
#'
#' Function called in the `validate_submission()` function, only if the
#' submission contains `"sample"` output type.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter n
#'@export
test_sample <- function(df, model_task, pairing_col = "horizon",
                        verbose = TRUE, verbose_col = NULL) {
  # Prerequisite
  or_pair <- dplyr::select(df, -dplyr::contains("output_type"), -value,
                           -dplyr::contains("run_grouping"),
                           -dplyr::contains("stochastic_run")) %>%
    as.list() %>%
    purrr::map(unique) %>%
    purrr::keep(function(x) length(x) == 1) %>%
    names()

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
            paste0("\U000274c Error 905: All the groups should contains the ",
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
            sample_value <- NA # nocov
          }
        } else {
          sample_value <- NA
        }

        # - sample id should contains the expected number of repetition
        # for each group (task_ids)
        # For pairing information: test if only minimum pairing repeated the
        # expected number of times
        sample_unique <- pairing_test(df_sample, x, or_pair, pairing_col)
        # Add pairing information
        if (verbose) {
          pair_inf <- verbose_pairing(df_sample, x, NULL, n_sample,
                                      verbose_col = c(verbose_col)) %>%
            purrr::map(unique)
        } else {
          pair_inf <- NA
        }
        # - result output
        test_sample <- unique(na.omit(c(sample_value, unlist(sample_unique),
                                        sample_type, pair_inf)))
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
          test_sample <-  NA # nocov
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
