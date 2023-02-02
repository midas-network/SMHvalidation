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
#'@details  This function contains 4 tests:
#'\itemize{
#'  \item{Quantiles: }{The `quantile` column matches the expected quantiles}
#'  \item{Required quantiles: }{The projection should contain all required
#'   quantiles. It is accepted to submit less quantiles but the
#'   function will return a warning and the submission will not be included
#'   in the Ensembles.}
#'  \item{Value: }{The `value` associated with each `quantile` is increasing as
#'  the quantile increased. For example, if quantile 0.01 = 5 than quantile
#'  0.5 should be equal or greater than 5.}
#'  \item{Target: }{All targets required to have quantiles information have all
#'  required quantiles for each group id (scenario/location/horizon/etc.)}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr filter mutate
#'@importFrom purrr discard keep map
#'
#'@export
test_quantiles <- function(df, model_task) {
  # Prerequisite
  req_quantile <- model_task$output_types$quantile$type_id$required
  opt_quantile <- model_task$output_types$quantile$type_id$optional
  all_quantile <- unique(c(req_quantile, opt_quantile))
  df <- data.table::data.table(df)[type == "quantile"]
  sub_quantile <- unlist(distinct(df[, "type_id"]))

  # - test all quantiles value are expected
  if (isFALSE(all(all_quantile %in% sub_quantile))) {
    if (all(req_quantile %in% df[, "type_id"])) {
      qvalues_test <- NA
    } else {
      qvalues_test <-  paste0(
        "\U000274c Error 402: At least one quantile is missing: ",
        paste(req_quantile[!req_quantile %in% sub_quantile], collapse = ", "),
        ". Please verify")
    }
  } else {
    qvalues_test <- NA
  }

  # - test submission does not have additional quantiles
  if (isFALSE(all(sub_quantile %in% all_quantile))) {
    qadd_test <- paste0(
      "\U000274c Error 407: At least one additional quantile was provided in",
      " the submission: ",  paste(sub_quantile[!sub_quantile %in% all_quantile],
                                  collapse = ", "), ". Please verify")
  } else {
    qadd_test <- NA
  }

  # target(s) contains all the required quantiles
  sel_group <- names(model_task$task_ids)
  df[, sel := ifelse(all(req_quantile %in% type_id), 0, 1), by = sel_group]
  df_test <- df[sel > 0]
  if (dim(df_test)[1] > 0) {
    err_groups <- df_test %>% dplyr::select(-sel, -value, -type, -type_id) %>%
      dplyr::distinct() %>% tidyr::unite("group", dplyr::everything(),
                                         sep = ", ") %>% unlist()
    qmissing_test <-  paste0(
      "\U0001f7e1 Warning 406: At least one quantile is missing from the ",
      " submission, please verify: ", err_groups, ". The file will be accepted",
      " but might not be included in the Ensembles.")
  } else {
    qmissing_test <- NA
  }
  if (length(na.omit(unlist(qmissing_test))) > 100) {
    qmissing_test <- paste0(unique(unlist(purrr::map(strsplit(
      qmissing_test, "please verify: "), 1))), length(qmissing_test),
      " unique groups have been identified with this issue. For example: \n",
      paste("group: ", head(purrr::map(
        strsplit(qmissing_test, "verify: "), 2),3), collapse = ";\n"),
      "; \netc.")
  }

  # - value increases with the quantiles
  df_qval <- df[order(type_id)][, !"sel"]
  df_qval[ , diff := value - data.table::shift(value, 1, type = "lag"),
           by = sel_group]
  df_qval <- df_qval[diff < 0]
  if (dim(df_qval)[1] > 0) {
    err_groups <- df_qval %>% dplyr::select(-diff, -value, -type, -type_id) %>%
      dplyr::distinct() %>% tidyr::unite("group", dplyr::everything(),
                                         sep = ", ") %>% unlist()
    qval_test <-   paste0(
      "\U000274c Error 403: Quantiles values are not increasing with ",
      "quantiles, please verify: ", err_groups)
  } else {
    qval_test <- NA
  }
  if (length(na.omit(unlist(qval_test))) > 100) {
    qval_test <- paste0(unique(unlist(purrr::map(strsplit(
      qval_test, "please verify: "), 1))), length(qval_test),
      " unique groups have been identified with this issue. For example: \n",
      paste("group: ", head(purrr::map(
        strsplit(qval_test, "verify: "), 2),3), collapse = ";\n"),
      "; \netc.")
  }

  quantiles_test <- na.omit(c(qvalues_test, qadd_test, qmissing_test,
                              qval_test))
  quantiles_test <- unique(quantiles_test)
  if (length(quantiles_test) == 0)
    quantiles_test <- paste0("No errors or warnings found on quantiles values ",
                             "and format")

  return(quantiles_test)

}
