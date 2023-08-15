#' Runs Validation Checks on the sample column
#'
#' Validate Scenario Modeling Hub submissions: test if the  `sample` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param model_task list containing round information for each id columns
#' and model output (type, format, etc.)
#'
#'@details  This function contains 3 tests:
#'\itemize{
#'  \item{sample value: }{The submission should contain a sample column with
#'  expected value.}
#'  \item{unique sample: }{The submission should contain a unique sample
#'  identifier for each scenario/target/location/horizon (age_group) group.}
#'  \item{sample integer: }{The submission should contain a sample column with
#'  integer only.}
#'  \item{required sample:}{If a target expected sample type output, the
#'  submission should contain sample type output for this target}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter
#'@export
test_sample <- function(df, model_task) {

  test_sample <- lapply(model_task, function(x) {
    if ("sample" %in% names(x$output_type)) {
      # prerequisite
      df_sample <- data.table::data.table(
        df)[output_type == "sample" &
              target %in% unique(unlist(x$task_ids$target))]
      vector_sample <- unlist(unique(df_sample[, output_type_id]))
      task_ids <- x$task_ids
      # - sample column should be an integer
      if (dim(df_sample)[1] > 0) {
        if (isFALSE(all(is.wholenumber(vector_sample)))) {
          sample_type <-  paste0(
            "\U000274c Error 903: The column 'output_type_id' should contains ",
            "integer values only for type 'sample'. Please verify")
        } else {
          sample_type <- NA
        }

        df_sample$output_type_id <- as.integer(df_sample$output_type_id)
        vector_sample <- as.integer(vector_sample)
        # - sample output_type_id column contains the expected value
        exp_sample <- as.numeric(unique(c(
          x$output_type$sample$output_type_id$required,
          x$output_type$sample$output_type_id$optional)))
        test_df <- dplyr::filter(df_sample,  output_type_id < min(exp_sample) |
                                   output_type_id > max(exp_sample))
        if (dim(test_df)[1] > 0 | any(grepl(
          "\\.", unlist(unique(df_sample[ , output_type_id]))))) {
          sample_value <-  paste0(
            "\U0001f7e1 Warning 901: The column 'output_type_id' should contains ",
            "integer values between ", min(exp_sample), " and ",
            max(exp_sample), " (included) for the type 'sample', please verify.")
        } else {
          sample_value <- NA
        }
        if (length(vector_sample) < length(unlist(
          x$output_type$sample$output_type_id))) {
          sample_value <- c(
            sample_value,
            paste0("\U0001f7e1 Warning 901: The column 'output_type_id' contains",
                   " less unique `sample` ID then expected. Up to ",
                   length(unique(unlist(x$output_type$sample$output_type_id))),
                   " unique 'samples' for each scenario/target/location",
                   "(/age_group) can be submitted."))
        }

        # - sample id should be unique for each group (task_ids)
        sel_group <- c(names(task_ids), "output_type", "output_type_id")
        df_test <- data.table::data.table(df_sample)
        df_test <- df_test[,.N, by = sel_group]
        df_test <- df_test[N > 1]
        if (dim(df_test)[1] > 0) {
          err_groups <- df_test %>% dplyr::select(-N) %>% dplyr::distinct() %>%
            tidyr::unite("group", dplyr::everything(), sep = ", ") %>% unlist()
          sample_unique <- paste0(
            "\U000274c Error 902: Each scenario/target/location (age_group, ",
            "etc.) group should have an unique sample identifier, please ",
            "verify: ", err_groups)
        } else {
          sample_unique <- NA
        }
        if (length(na.omit(unlist(sample_unique))) > 100) {
          sample_unique <- paste0(unique(unlist(purrr::map(strsplit(
            sample_unique, "please verify: "), 1))), length(sample_unique),
            " unique groups have been identified with this issue. For example:",
            " \n", paste("group: ", head(purrr::map(strsplit(
              sample_unique, "verify: "), 2), 3), collapse = "; \n"),
            "; \netc.")
        }

        # - result output
        test_sample <- unique(na.omit(c(sample_value, unlist(sample_unique),
                                        sample_type)))
      } else {
        if (!is.null(x$task_ids$target$required) &
            !is.null(x$output_type$sample$output_type_id$required)) {
          test_sample <- paste0(
            "\U000274c Error 904: Samples are expected in the submission for ",
            "the target(s): ", paste(unique(unlist(x$task_ids$target$required)),
                                     collapse = ", "), ". please verify.")
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
