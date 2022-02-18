#' Runs Validation Checks on the Quantiles type and value columns
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `quantile` and `type` columns contain the expected information and value
#' and the projection value increases with the quantiles.
#'
#'@param df data frame to test
#'@param round numeric corresponding to the current round number
#'
#'@details  This functions contains 3 tests:
#'\itemize{
#'  \item{"Quantiles": }{The `quantile` column matches the expected quantiles:
#'  0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55,
#'   0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99 and/or 1.}
#'  \item{"Number": }{The projection should contain at least the 23 quantiles
#'   (0 and 1 are optional). It is accepted to submit less quantiles but the
#'   function will return a warning and the submission will not be included
#'   in the Ensembles.}
#'  \item{"Value": }{The `model_projection_date`
#'  column contains a unique date value matching the date in the name of the
#'  submission file}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter
#'
#'@export
test_quantiles <- function(df, round) {
  #Vector of expected quantile (23 different quantile for all rounds except
  # round 10: expected all 23 quantiles + 2 new quantiles: 0 & 1)
  quantiles <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
                 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975,
                 0.99)

  if (round > 9) {
    n_quantile <- 26 # Number of quantiles + NA for the "point" value (25 + 1)
    quantiles <- c(0, quantiles, 1) # update vector of quantiles to add 0 and 1
  } else {
    n_quantile <- 24 # Number of quantiles + NA for the "point" value (23 + 1)
  }
  # - quantiles values (should be the same as in the GitHub)
  if (isFALSE(all(na.omit(df$quantile) %in% quantiles))) {
    qvalues_test <- paste0(
      "\U000274c Error: ", unique(na.omit(df$quantile))[!unique(na.omit(
        df$quantile)) %in% quantiles], " is not an accepted quantile.")
  } else {
    qvalues_test <- NA
  }
  # - number of quantiles
  if (isTRUE(length(unique(df$quantile)) < n_quantile)) {
    if (isFALSE(all(quantiles[!quantiles %in% df$quantile] %in% c(0, 1)))) {
      qnum_test <- paste0(
        "\U0001f7e1 Warning: Expected number of 'quantile' is at least 23 + NA",
        " (+ 2 optional quantiles: 0, 1) unique values. The projection ",
        "contains: ", length(unique(df$quantile)), " unique values of ",
        "`quantile`. The file will be accepted but might not be included in ",
        "the Ensembles.")
    } else {
      qnum_test <- NA
    }
  } else {
    qnum_test <- NA
  }
  # - value increases with the quantiles
  lst_df <- split(df, list(df$scenario_id, df$location, df$target))
  qval_test <- lapply(lst_df, function(x) {
    group <- paste0("target: ", unique(x$target), ", location: ",
                    unique(x$location), ", scenario: ", unique(x$scenario_id))
    dfstl <- dplyr::filter(x, type != "point")
    sel_quantile <- sort(dfstl$quantile)
    for (i in 1:(length(sel_quantile) - 1)) {
      value <- dfstl[dfstl$quantile == sel_quantile[i], "value", TRUE]
      n_value <- dfstl[dfstl$quantile == sel_quantile[i+1], "value", TRUE]
      suppressWarnings(if (n_value < value) {
        qval_test <- paste0(
          "\U000274c Error: Quantiles values are not increasing with quantiles",
          ", please verify: ", sel_quantile[i], " and ", sel_quantile[i+1],
          " for the group: ", group)
      } else {
        qval_test <- NA
      })
    }
    return(qval_test)
  })

  quantiles_test <- na.omit(c(qvalues_test, qnum_test, unlist(qval_test)))
  if (length(quantiles_test) == 0)
    quantiles_test <- paste0("No errors or warnings found on quantiles values ",
                             "and format")

  return(quantiles_test)

}
