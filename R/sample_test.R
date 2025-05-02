#' @importFrom hubValidations check_tbl_spl_compound_taskid_set try_check
#' check_tbl_spl_compound_tid check_tbl_spl_non_compound_tid is_any_error
#' check_tbl_spl_n capture_check_cnd
#' @importFrom purrr map map_vec flatten
#' @importFrom dplyr setdiff filter
#' @importFrom data.table setDT .N
#' @noRd
sample_test <- function(checks, tbl_chr, round_id, file_path, hub_path,
                        path, pair = NULL, js_def = NULL) {
  if (!is.null(pair)) {
    if (!all(tbl_chr$output_type %in% "sample"))
      tbl_chr <- dplyr::filter(tbl_chr, .data[["output_type"]] == "sample")
    out_task <- purrr::map(js_def, "output_type")
    out_spl <- out_task[purrr::map_vec(out_task, ~ any(names(.x) == "sample"))]
    out_spl <- purrr::map(out_spl, "sample")
    # Compound ID
    id_set <- purrr::map(out_spl,
                         ~ .x[["output_type_id_params"]]$compound_taskid_set)
    pair_set <- purrr::map(id_set,
                           ~ dplyr::setdiff(names(js_def[[1]]$task_ids), .x))
    pair_vect <- unique(unlist(c(pair$run_group, pair$sto_group)))
    check <- all(purrr::map_vec(pair_set,  ~ all(.x %in% pair_vect)))
    if (!check) {
      missing <- unique(unlist(purrr::map(pair_set,  ~.x[!.x %in% pair_vect])))
      miss <- missing[purrr::map_vec(missing,
                                     ~ length(unique(tbl_chr[[.x]])) > 1)]
      if (length(miss) > 0) {
        details <- paste0("Missing column(s): ", paste(miss,  collapse = ", "))
      } else {
        check <- TRUE
        add_p <- missing[purrr::map_vec(missing,
                               ~ length(unique(tbl_chr[[.x]])) == 1)]
        details <- paste("Pairing information: ", paste(c(pair_vect, add_p),
                                                          collapse = ", "))
      }
    } else {
      details <- paste("Pairing information: ",
                       paste(pair_vect, collapse = ", "))
    }
    checks$spl_compound_taskid_set <- hubValidations::capture_check_cnd(
      check = check,
      file_path = file_path,
      msg_subject = "All samples in a model task",
      msg_attribute = "to single, unique compound task ID set that matches or is
    coarser than the configured {.var compound_taksid_set}.",
      msg_verbs = c("conform", "do not conform"),
      details = details,
      error = TRUE
    )

    # Unique
    test <- dplyr::filter(tbl_chr, .data[["output_type"]] == "sample")
    id_vect <- unique(unlist(c(pair$run_group, pair$sto_group)))
    check <- data.table::setDT(test)[, .N, by = c(id_vect, "output_type_id")]
    check <- dplyr::filter(check, .data[["N"]] > 1)
    checks$spl_compound_tid <- hubValidations::capture_check_cnd(
      check = nrow(check) == 0,
      file_path = file_path,
      msg_subject = "Each sample compound task ID",
      msg_attribute = "single, unique value.",
      msg_verbs = c("contains", "does not contain"),
      error = TRUE
    )

    checks$spl_non_compound_tid <- hubValidations::capture_check_info(
      file_path = file_path,
      msg = "Task ID combinations of non compound task id values not tested"
    )

    # N samples
    min_spl <- out_spl[[1]]$output_type_id_params$min_samples_per_task
    max_spl <- out_spl[[1]]$output_type_id_params$max_samples_per_task
    check <- all(min_spl <= pair$n_sample) & all(pair$n_sample <= max_spl)
    err <- TRUE
    if (check && (length(pair$n_sample) > 1)) {
      check <- err <- FALSE
    }
    checks$spl_n <- hubValidations::capture_check_cnd(
      check = check,
      error = err,
      file_path = file_path,
      msg_subject = "Required unique number of samples per compound idx task",
      msg_attribute = NULL,
      msg_verbs = c("present.", "not present."),
      details = paste0("Only one number of samples from: ", min_spl, " to ",
                       max_spl, " are accepted. Submission contains: ",
                       paste(pair$n_sample, collapse = ", "))
    )
  } else {
    checks$spl_compound_taskid_set <-
      try_check(check_tbl_spl_compound_taskid_set(tbl_chr, round_id = round_id,
                                                  file_path = file_path,
                                                  hub_path = hub_path,
                                                  derived_task_ids = NULL),
                path)
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
      try_check(check_tbl_spl_non_compound_tid(
        tbl_chr, round_id = round_id, file_path = file_path,
        hub_path = hub_path, compound_taskid_set = cmd_tkid_set,
        derived_task_ids = NULL
      ), path)
    if (is_any_error(checks$spl_non_compound_tid)) {
      return(checks)
    }
    checks$spl_n <-
      try_check(check_tbl_spl_n(tbl_chr, round_id = round_id,
                                file_path = file_path, hub_path = hub_path,
                                compound_taskid_set = cmd_tkid_set,
                                derived_task_ids = NULL), path)
  }

  checks
}
