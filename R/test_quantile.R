#' Runs Validation Checks on the Quantiles type and value columns
#'
#' Validate Scenario Modeling Hub submissions: test if the
#' `quantile` and `type` columns contain the expected information and value
#' and the projection value increases with the quantiles.
#'
#'@param df data frame to test
#'@param js_def list containing round definitions: number and names of columns,
#' target names, ...
#'
#'@details  This function contains 6 tests:
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
#'  \item{Unique value: }{For the target(s) requiring quantiles information,
#'  not all the quantiles per scenario, target and location (and age_group if
#'  necessary) are equal.}
#'  \item{1 quantile: }{For the target(s) requiring quantiles information, all
#'  quantiles per scenario, target and location (and age_group if necessary) are
#'   unique.}
#'  \item{Target: }{All targets required to have quantiles information have all
#'  expected quantiles.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter mutate
#'@importFrom purrr discard keep map
#'
#'@export
test_quantiles <- function(df, js_def) {
  #Vector of expected quantile
  quantiles <- sort(unique(unlist(js_def$quantiles)))
  n_quantile <- length(quantiles) + 1

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
    if (isFALSE(all(quantiles[!quantiles %in% df$quantile] %in%
                    js_def$quantiles$optional))) {
      qnum_test <- paste0(
        "\U0001f7e1 Warning 402: Expected number of 'quantile' is at least ",
        length(js_def$quantiles$required), " + NA ",
        if (all(!is.na(js_def$quantiles$optional))) {
          paste0("(+ ", length(js_def$quantiles$optional),
                 " optionals quantiles: ", paste(js_def$quantiles$optional,
                                                 collapse = ", "),")")
          },
        " unique values. The projection ",
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

  # target name requiring quantiles
  target_quan <- purrr::map(c(js_def$targets$required, js_def$targets$optional),
                            "quantiles")
  target_quan <- purrr::discard(target_quan, is.null)
  target_quan <- purrr::keep(purrr::map(target_quan, "required"),
                            function(x) "all" %in% x)
  target_quan <- names(target_quan)

  # - required targets contains quantile
  qmissing_test <- lapply(target_quan, function(x) {
    test_df <- dplyr::filter(df, grepl(x, target), type == "quantile",
                             !is.na(quantile))
    if (isFALSE(all(unique(test_df$quantile) %in% js_def$quantiles$required))) {
      qmissing_test <-  paste0(
        "\U0001f7e1 Warning 406: The target", x, " is  missing quantiles ",
        "information, please verify.The file will be accepted but might not be",
        " included in the Ensembles if required quantiles are missing.")
  } else {
      qmissing_test <- NA
    }
  })

  # - value increases with the quantiles
  df2 <- dplyr::filter(df, grepl(paste(target_quan, collapse = "|"), target))
  df2 <- dplyr::filter(df2, !is.na(value))
  ### automatic fix location to avoid issue and error message
  df2 <-  dplyr::mutate(
    df2, location = ifelse(nchar(location) == 1, paste0("0", location),
                           location))
  sel_group <- grep(
    "value|target_end_date|type|quantile|model_projection_date|scenario_name",
    js_def$column_names, invert = TRUE, value = TRUE)
  lst_df <-  split(df2, as.list(df2[,sel_group]))
  lst_df <- purrr::discard(lst_df, function(x) dim(x)[[1]] < 1)
  qval_test  <- lapply(lst_df, function(x) {
    group <- paste(names(unique(x[, sel_group])), ":", unique(x[, sel_group]),
                   collapse = ", ")
    dfstl <- dplyr::filter(x, grepl("quantile", type))
   # if (length(unique(dfstl$value)) == 1) {
   #   if (unique(dfstl$value) != 0) {
   #     qval_test_tot <- paste0(
   #       "\U000274c Error 405: All the quantiles seem to be ",
   #       "equal to a unique value for the group: ", group)
   #   } else {
   #     qval_test_tot <- NA
   #   }
   # } else {
      sel_quantile <- sort(unique(dfstl$quantile))
      qval_test_tot <- NULL
      for (i in 1:(length(sel_quantile) - 1)) {
        value <- dfstl[dfstl$quantile == sel_quantile[i], "value", TRUE]
        n_value <- dfstl[dfstl$quantile == sel_quantile[i+1], "value", TRUE]
        if (length(n_value) != 1) {
          qval_test <- paste0(
            "\U000274c Error 404: The quantile ", sel_quantile[i + 1], ", is ",
            "not associated with an unique value for the group: ", group)
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
    #}
    qval_test_tot <- unique(na.omit(qval_test_tot))
    if (length(qval_test_tot) == 0) qval_test_tot <- NA
    return(qval_test_tot)
  })
  if (length(na.omit(unlist(qval_test))) > 100) {
    qval_test <- unique(na.omit(unlist(qval_test))) %>%
      gsub(" target : \\d{1,2} wk ahead ", " target: ", .) %>% unique
  }


  quantiles_test <- na.omit(c(qvalues_test, qnum_test, unlist(qval_test)))
  quantiles_test <- unique(quantiles_test)
  if (length(quantiles_test) == 0)
    quantiles_test <- paste0("No errors or warnings found on quantiles values ",
                             "and format")

  return(quantiles_test)

}
