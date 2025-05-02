# nocov start
#'@importFrom purrr map compact
#'@importFrom dplyr group_split
pairing_test <- function(df_sample, m_task, or_pair, pairing_col) {
  if (length(unique(df_sample$run_grouping)) > 1) {
    run_group <- purrr::map(dplyr::group_split(df_sample,
                                               .data[["run_grouping"]]),
                            paired_info,
                            rm_col = c("stochastic_run", "output_type_id",
                                       "output_type", "value"),
                            tasks_list = m_task$task_ids) |>
      unique() |>
      purrr::map(c, or_pair)
  } else {
    run_group <- NULL
  }
  if (length(unique(df_sample$stochastic_run)) > 1) {
    sto_group <- purrr::map(dplyr::group_split(df_sample,
                                               .data[["stochastic_run"]]),
                            paired_info, c("run_grouping",
                                           "output_type_id",
                                           "output_type", "value"),
                            tasks_list = m_task$task_ids) |>
      unique() |>
      purrr::map(c, or_pair)
  } else {
    sto_group <- NULL
  }
  sub_pair <- purrr::compact(c(sto_group, run_group))
  miss_col <- purrr::map(sub_pair,
                         function(l) pairing_col[!(pairing_col %in% l)])
  miss_col <- unique(unlist(miss_col))
  if (length(miss_col) > 0) {
    message("\U000274c Error: The minimal accepted grouping includes the ",
            "column(s): ", paste(pairing_col, collapse = ", "),
            ", the column(s): ", paste(miss_col, collapse = ", "),
            " seem to be missing (or is missing a sub-group), please  verify.")
  }
  invisible(NULL)
}

#' Runs Validation Checks on the sample column
#'
#' **DEPRECATED** <br><br>
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
#'@details  This function tests:
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
#' `model_task` should match a specific round model tasks from the
#' `tasks.json` associated with the hub. The json is expected to follow
#' the [hubverse](https://hubverse.io/en/latest/user-guide/hub-config.html)
#' schema at least version 5.0
#'
#' As the function was deprecated, it will not be updated anymore. It was
#' updated a last time to match the 5.0 hubverse schema version. However, it
#' might returns duplicated message output.
#'
#'@importFrom dplyr select contains all_of mutate mutate_if left_join filter
#'@importFrom dplyr group_by across n
#'@importFrom purrr map keep
#'@importFrom stats setNames
#'@export
test_sample <- function(df, model_task, pairing_col = "horizon",
                        verbose = TRUE, verbose_col = NULL) {

  warning("Function deprecated")

  # Prerequisite
  or_pair <- dplyr::select(df, -dplyr::all_of(c("output_type", "value")),
                           -dplyr::contains("run_grouping"),
                           -dplyr::contains("stochastic_run")) |>
    as.list() |>
    purrr::map(unique) |>
    purrr::keep(function(x) length(x) == 1) |>
    names()

  lapply(model_task, function(x) {
    if ("sample" %in% names(x$output_type)) {
      # prerequisite
      df_sample <-
        dplyr::mutate(df, origin_date = as.Date(.data[["origin_date"]]))
      tasks_list <- setNames(lapply(names(x$task_ids),
                                    function(z) unique(unlist(x$task_id[[z]]))),
                             names(x$task_ids))
      test_df <- expand.grid(tasks_list) |>
        dplyr::mutate_if(is.factor, as.character) |>
        dplyr::mutate(origin_date = as.Date(.data[["origin_date"]]))
      test_df$sel <- 1
      df_sample <- dplyr::left_join(df_sample, test_df,
                                    by = names(tasks_list)) |>
        dplyr::filter(.data[["sel"]] == 1,
                      .data[["output_type"]] == "sample") |>
        dplyr::select(-dplyr::all_of(c("sel")))
      vector_sample <- unlist(unique(df_sample[, "output_type_id"]))
      task_ids <- x$task_ids
      # - sample column should be an integer
      if (dim(df_sample)[1] > 0) {
        if (isFALSE(all(is_wholenumber(vector_sample)))) {
          message("\U000274c Error: The column 'output_type_id' should ",
                  "contains integer values only for type 'sample'. ",
                  "Please verify")
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
          message("\U000274c Error: All the groups should contains the ",
                  "same number of trajectories per group. Please verify.")
        } else if (n_sample != max(exp_sample)) {
          if (n_sample < max(as.numeric(req_sample_max))) {
            message("\U0001f7e1 Warning: The column 'output_type_id' ",
                    "should contains at least ",
                    max(as.numeric(req_sample_max)),
                    " (included) trajectories per group for the type 'sample'",
                    ", the submission file contains: ",
                    paste(n_sample, collapse = ", "),
                    " trajectories per group. Please verify.")
          } else if (n_sample > max(exp_sample)) {
            message("\U0001f7e1 Warning: The column 'output_type_id' ",
                    "should contains a maximum of ", max(exp_sample),
                    " (included) trajectories per group for the type 'sample'",
                    ", the submission file contains: ",
                    paste(n_sample, collapse = ", "),
                    " trajectories per group. Please verify.")
          }
        }
        # - sample id should contains the expected number of repetition
        # for each group (task_ids)
        # For pairing information: test if only minimum pairing repeated the
        # expected number of times
        pairing_test(df_sample, x, or_pair, pairing_col)
        # Add pairing information
        if (verbose) {
          pair_inf <- verbose_pairing(df_sample, x, NULL, NULL,
                                      n_sample = n_sample,
                                      verbose_col = c(verbose_col)) |>
            purrr::map(unique)
          message(pair_inf)
        }
      } else {
        if (!is.null(x$task_ids$target$required) &
              ("sample" %in% names(x$output_type))) {
          if (x$output_type$sample$is_required) {
            message("\U000274c Error: Samples are expected in the ",
                    "submission for the target(s): ",
                    paste(unique(unlist(x$task_ids$target$required)),
                          collapse = ", "), ". please verify.")
          }
        }
      }
    }
    invisible(NULL)
  })
  invisible(NULL)
}
# nocov end
