#' @importFrom hubValidations check_tbl_spl_compound_taskid_set try_check
#' check_tbl_spl_compound_tid check_tbl_spl_non_compound_tid is_any_error
#' check_tbl_spl_n
#' @noRd
sample_test <- function(checks, tbl_chr, round_id, file_path, hub_path,
                        path) {
  checks$spl_compound_taskid_set <-
    try_check(check_tbl_spl_compound_taskid_set(tbl_chr, round_id = round_id,
                                                file_path = file_path,
                                                hub_path = hub_path,
                                                derived_task_ids = NULL), path)
  cmd_tkid_set <- checks$spl_compound_taskid_set$compound_taskid_set
  checks$spl_compound_tid <-
    try_check(check_tbl_spl_compound_tid(tbl_chr, round_id = round_id,
                                         file_path = file_path,
                                         hub_path = hub_path,
                                         compound_taskid_set = cmd_tkid_set,
                                         derived_task_ids = NULL), path)
  if (is_any_error(checks$spl_compound_tid)) {
    return(checks)
  }
  checks$spl_non_compound_tid <-
    try_check(check_tbl_spl_non_compound_tid(tbl_chr, round_id = round_id,
                                             file_path = file_path,
                                             hub_path = hub_path,
                                             compound_taskid_set = cmd_tkid_set,
                                             derived_task_ids = NULL), path)
  if (is_any_error(checks$spl_non_compound_tid)) {
    return(checks)
  }
  checks$spl_n <-
    try_check(check_tbl_spl_n(tbl_chr, round_id = round_id,
                              file_path = file_path, hub_path = hub_path,
                              compound_taskid_set = cmd_tkid_set,
                              derived_task_ids = NULL), path)
  return(checks)
}
