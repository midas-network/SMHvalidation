#' Runs Validation Checks on the samplecolumn
#'
#' Validate Scenario Modeling Hub submissions: test if the  `sample` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param task_ids data.frame containing round information for each id columns
#'
#'@details  This function contains 3 tests:
#'\itemize{
#'  \item{sample value: }{The submission should contain a sample column with
#'  value between 0 and 100.}
#'   \item{unique sample: }{The submission should contain a unique sample
#'   identifier for each scenario/target/location (age_group) group.}
#'    \item{sample integer: }{The submission should contain a sample column with
#'  integer only.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter
#'@export
test_sample <- function(df, task_ids) {
  # prerequisite
  df_sample <- dplyr::filter(df, type %in% "sample")
  vector_sample <- unlist(distinct(df_sample[ ,"type_id", FALSE]))
  # - sample column should be an integer
  # Function from ?is.integer() function documentation
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  if (isFALSE(all(is.wholenumber(vector_sample)))) {
    sample_type <-  paste0(
      "\U000274c Error 903: The column 'sample' should contains integer values",
      " only. Please verify")
  } else {
    sample_type <- NA
  }

  # - sample type_id column contains the expected value
  exp_sample <- as.numeric(unique(na.omit(c(
    model_task$output_types$sample$type_id$required[[1]],
    model_task$output_types$sample$type_id$optional[[1]]))))
  test_df <- dplyr::filter(df_sample,  type_id < min(exp_sample) |
                             type_id > max(exp_sample))
  if (dim(test_df)[1] > 0 | any(grepl(
    "\\.", unlist(distinct(df[ ,"type_id", FALSE]))))) {
    sample_value <-  paste0(
      "\U0001f7e1 Warning 901: The column 'sample' should contains integer",
      " values between ", min(exp_sample), " and ", max(exp_sample),
      " (included), please verify.")
  } else {
    sample_value <- NA
  }
  if (max(vector_sample) < max(
    model_task$output_types$sample$type_id$required[[1]])) {
    sample_value <- c(
      sample_value,
      paste0("\U0001f7e1 Warning 901: The column 'sample' contains less ",
             "`sample` then expected. Up to 100 'samples' for each scenario/",
             "target/location(/age_group) can be submitted."))
  }

  # - sample id should be unique for each group (task_ids)
  sel_group <- c(names(task_ids), "type", "type_id")
  df_test <- data.table::data.table(df_sample)
  df_test <- df_test[,.N, by = sel_group]
  df_test <- df_test[N > 1]
  if (dim(df_test)[1] > 0) {
    err_groups <- df_test %>% dplyr::select(-N) %>% dplyr::distinct() %>%
      tidyr::unite("group", dplyr::everything(), sep = ", ") %>% unlist()
     sample_unique <- paste0(
        "\U000274c Error 902: Each scenario/target/location (age_group, etc.)",
        " group should have an unique sample identifier, please verify: ",
        err_groups)
  } else {
    sample_unique <- NA
  }
  if (length(na.omit(unlist(sample_unique))) > 100) {
    sample_unique <- paste0(unique(unlist(purrr::map(strsplit(
      sample_unique, "please verify: "), 1))), length(sample_unique),
      " unique groups have been identified with this issue. For example: \n",
      paste("group: ", head(purrr::map(strsplit(sample_unique, "verify: "), 2),
                            3), collapse = "; \n"), "; \netc.")
  }

  # - result output
  test_sample <- unique(na.omit(c(sample_value, unlist(sample_unique),
                                  sample_type)))
  if (length(test_sample) == 0)
    test_sample <- "No errors or warnings found on Sample"

  return(test_sample)
}
