#' Runs Validation Checks on the samplecolumn
#'
#' Validate Scenario Modeling Hub submissions: test if the  `sample` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param js_def list containing round definitions: number and names of columns,
#' target names, ...
#'
#'@details  This function contains 2 tests:
#'\itemize{
#'  \item{sample value: }{The submission should contain a sample column with
#'  value between 0 and 100.}
#'   \item{unique samplee: }{The submission should contain a unique sample
#'   identifier for each scenario/target/location (age_group) group.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter
#'@export
test_sample <- function(df, js_def) {
  # - sample column should be an integer
  # Function from ?is.integer() function documentation
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  if (any(!is.wholenumber(df$sample))) {
    sample_type <-  paste0(
      "\U000274c Error 903: The column 'sample' should contains integer values",
      " only. Please verify")
  } else {
    sample_type <- NA
  }

  # - sample column contain value from 1 to 100 (warning if under 100 or over
  # 100)
  test_df <- dplyr::filter(df, sample < 1 | sample > 100)
  if (dim(test_df)[1] > 0 | any(grepl("\\.", df$sample))) {
    sample_value <-  paste0(
      "\U0001f7e1 Warning 901: The column 'sample' should contains integer",
      " values between 1 and 100 (included), please verify.")
  } else {
    sample_value <- NA
  }

  if (max(df$sample) < 100) {
    sample_value <- c(
      sample_value,
      paste0("\U0001f7e1 Warning 901: The column 'sample' contains less ",
             "`sample` then expected. Up to 100 'samples' for each scenario/",
             "target/location(/age_group) can be submitted."))
  }

  sel_group <- grep(
    "value|target_end_date|model_projection_date|scenario_name",
    js_def$column_names, invert = TRUE, value = TRUE)
  lst_df <-  split(df, as.list(df[,sel_group]), drop = TRUE)
  lst_df <- purrr::discard(lst_df, function(x) dim(x)[[1]] < 1)
  lst_df_test <- purrr::discard(lst_df, function(x) dim(x)[[1]] < 2)
  if (length(lst_df_test) > 0) {
    sample_unique <- lapply(lst_df_test, function(x) {
      group <- paste(names(unique(x[, sel_group])), ":", unique(x[, sel_group]),
                     collapse = ", ")
      sample_unique <-  paste0(
        "\U000274c Error 902: Each scenario/target/location (age_group) group ",
        "should have an unique sample identifier, please verify: ", group)
    })
  } else {
    sample_unique <- NA
  }
  if (length(na.omit(unlist(sample_unique))) > 100) {
    sample_unique <- unique(na.omit(unlist(sample_unique))) %>%
      gsub(" target : \\d{1,2} wk ahead ", " target: ", .) %>% unique
  }

  test_sample <- unique(na.omit(c(sample_value, unlist(sample_unique),
                                  sample_type)))
  if (length(test_sample) == 0)
    test_sample <- "No errors or warnings found on Sample"

  return(test_sample)
}
