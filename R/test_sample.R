#' Runs Validation Checks on the samplecolumn
#'
#' Validate Scenario Modeling Hub submissions: test if the  `sample` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'
#'@details  This function contains 1 test:
#'\itemize{
#'  \item{sample value: }{The submission should contain a sample column with
#'  value between 0 and 100.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter
#'@export
test_sample <- function(df) {
  # - sample column should be an integer between 0 and 100,
  test_df <- dplyr::filter(df, sample < 1 | sample > 100)
  if (dim(test_df)[1] > 0 | any(grepl("\\.", df$sample))) {
    sample_value <-  paste0(
      "\U000274c Error 901: The column 'sample' contains integer value between",
      " 1 and 100 (included), please verify.")
  } else {
    sample_value <- NA
  }

  test_sample <- unique(na.omit(c(sample_value)))
  if (length(test_sample) == 0)
    test_sample <- "No errors or warnings found on Sample"

  return(test_sample)
}
