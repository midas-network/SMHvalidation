#' @importFrom hubValidations  try_check check_tbl_match_round_id
#' check_valid_round_id_col check_tbl_unique_round_id is_any_error
#' @noRd
round_id_test <- function(checks, df, file_path, hub_path, path) {
  checks$valid_round_id_col <-
    try_check(check_valid_round_id_col(df, file_path = file_path,
                                       hub_path = hub_path), path)
  checks$unique_round_id <-
    try_check(check_tbl_unique_round_id(df, file_path = file_path,
                                        hub_path = hub_path), path)
  if (is_any_error(checks$unique_round_id)) {
    return(checks)
  }

  checks$match_round_id <-
    try_check(check_tbl_match_round_id(df, file_path = file_path,
                                       hub_path = hub_path), path)
  if (is_any_error(checks$match_round_id)) {
    return(checks)
  }
  checks
}
