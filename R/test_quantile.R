#' Runs Validation Checks on the Quantiles type and value columns
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `quantile` and `type` columns contain the expected information and value
#' and the projection value increases with the quantiles.
#'
#'@param df data frame to test
#'@param round numeric corresponding to the current round number
#'
#'@details  This function contains 3 tests:
#'\itemize{
#'  \item{Quantiles: }{The `quantile` column matches the expected quantiles:
#'  0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55,
#'   0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99 and/or 1.}
#'  \item{Number: }{The projection should contain at least the 23 quantiles
#'   (0 and 1 are optional). It is accepted to submit less quantiles but the
#'   function will return a warning and the submission will not be included
#'   in the Ensembles.}
#'  \item{Value: }{The `value` associated with each `quantile` is increasing as
#'  the quantile increased. For example, if quantile 0.01 = 5 than quantile
#'  0.5 should be equal or greater than 5.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter %>%
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
      "\U000274c Error 401: ", unique(na.omit(df$quantile))[!unique(na.omit(
        df$quantile)) %in% quantiles], " is not an accepted quantile.")
  } else {
    qvalues_test <- NA
  }
  # - number of quantiles
  if (isTRUE(length(unique(df$quantile)) < n_quantile)) {
    if (isFALSE(all(quantiles[!quantiles %in% df$quantile] %in% c(0, 1)))) {
      qnum_test <- paste0(
        "\U0001f7e1 Warning 402: Expected number of 'quantile' is at least 23",
        " + NA (+ 2 optional quantiles: 0, 1) unique values. The projection ",
        "contains: ", length(unique(df$quantile)), " unique values of ",
        "`quantile`. The file is missing the quantile(s): '",
        paste(quantiles[!quantiles %in% df$quantile], collapse = ", "), "'. ",
        "The file will be accepted but might not be included in the Ensembles.")
    } else {
      qnum_test <- NA
    }
  } else {
    qnum_test <- NA
  }
  # - value increases with the quantiles
  target_sel <- c("inc death", "inc case", "cum death", "cum case",
              "inc hosp", "cum hosp", "inc inf")
  df2 <- dplyr::filter(df, grepl(paste(target_sel, collapse = "|"), target))
  lst_df <- split(df2, list(df2$scenario_id, df2$location, df2$target))
  qval_test <- lapply(lst_df, function(x) {
    group <- paste0("target: ", unique(x$target), ", location: ",
                    unique(x$location), ", scenario: ", unique(x$scenario_id))
    dfstl <- dplyr::filter(x, type != "point")

   # if (length(unique(dfstl$value)) == 1) {
   #    qval_test_tot <- paste0(
  #      "\U000274c Error 405: All the quantiles seem to be ",
  #      "equal to a unique value for the group: ", group)
  #  } else {
      sel_quantile <- sort(unique(dfstl$quantile))
      qval_test_tot <- NULL
      for (i in 1:(length(sel_quantile) - 1)) {
        value <- dfstl[dfstl$quantile == sel_quantile[i], "value", TRUE]
        n_value <- dfstl[dfstl$quantile == sel_quantile[i+1], "value", TRUE]

        if (length(n_value) != 1) {
          qval_test <- paste0(
            "\U000274c Error 404: The quantile ", sel_quantile[i + 1], ", is not",
            " associated with an unique value for the group: ", group)
        } else if (length(value) != 1) {
          qval_test <- paste0(
            "\U000274c Error 404: The quantile ", sel_quantile[i], ", is not ",
            "associated with an unique value for the group: ", group)
        } else {
          if (n_value < value) {
            qval_test <- paste0(
              "\U000274c Error 403: Quantiles values are not increasing with ",
              "quantiles, please verify: ", sel_quantile[i], " and ",
              sel_quantile[i+1], " for the group: ", group)
          } else {
            qval_test <- NA
          }
        }
        qval_test <- unique(qval_test)
        qval_test_tot <- unique(c(qval_test_tot, qval_test))
      }
   # }
    qval_test_tot <- unique(na.omit(qval_test_tot))
    if (length(qval_test_tot) == 0) qval_test_tot <- NA
    return(qval_test_tot)
  })

  if (length(qval_test) > 150) {
    qval_test <- unique(na.omit(unlist(qval_test))) %>%
      gsub(" target: \\d{1,2} wk ahead", "", .) %>% unique
  }


  quantiles_test <- na.omit(c(qvalues_test, qnum_test, unlist(qval_test)))
  if (length(quantiles_test) == 0)
    quantiles_test <- paste0("No errors or warnings found on quantiles values ",
                             "and format")

  return(quantiles_test)

}
