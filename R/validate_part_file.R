#' Validate Partitioned files level properties
#'
#' Function based on `hubValidations::validate_model_file()`, adapted to work
#' for partitioned SMH file format
#'
#' @param hub_path path to the repository contains the submissions files
#' and `tasks.json` file.
#' @param folder_path path to the folder containing partitioned submission
#' files. The path should be relative to the `hub_path`, model output folder.
#' @param partition character vector corresponding to the column names of each
#' path segments
#'
#' @details
#' See `hubValidations::validate_model_file()` on the tests performed in the
#' function. Additional tests includes checking the partition names and value
#' are matching the expected values.
#'
#' @importFrom hubValidations new_hub_validations capture_check_cnd is_any_error
#' parse_file_name check_valid_round_id check_submission_metadata_file_exists
#' check_file_name check_file_format
#' @importFrom hubUtils read_config get_round_ids
#' @importFrom purrr map map_vec list_transpose
#' @importFrom utils URLdecode
#'
#' @export
#'
#' @examples
#' hub_path <- system.file("extdata/exp/", package = "SMHvalidation")
#' path_2 <- "t3-mc"
#' SMHvalidation::validate_part_file(hub_path, path_2,
#'                                   c("origin_date", "target"))
#'
validate_part_file <- function(hub_path, folder_path, partition) {

  # Prerequisite
  checks <- new_hub_validations()
  full_path <- paste0(hub_path, "/model-output/", folder_path)

  # file exists
  test <- file.exists(dir(full_path, recursive = TRUE, full.names = TRUE))
  if (length(test) == 0) test <- FALSE
  checks$file_exists <-
    capture_check_cnd(check = all(test), file_path = folder_path,
                      msg_subject = "Files", error = TRUE,
                      msg_attribute = paste0("at path `",
                                             paste0("/model-output/",
                                                    folder_path), "`"),
                      msg_verbs = c("exist", "do not exist"))
  if (is_any_error(checks$file_exists)) {
    return(checks)
  }

  # Prerequisite
  all_files <- dir(full_path, recursive = TRUE)
  file_name <- gsub("0\\.", "\\.", unique(basename(all_files)))

  # Number of files
  checks$file_n <-
    capture_check_cnd(check = length(file_name) == 1,
                      file_path = folder_path,
                      msg_subject = "Files", error = TRUE,
                      msg_attribute = "the same name.",
                      msg_verbs = c("have", "does not have"))
  if (is_any_error(checks$file_n)) {
    return(checks)
  }

  # Prerequisite
  file_meta <- parse_file_name(file_name)
  round_id <- file_meta$round_id
  # Round id is correct
  checks$round_id_valid <- try_check(check_valid_round_id(round_id = round_id,
                                                          file_path = file_name,
                                                          hub_path = hub_path),
                                     folder_path)
  if (is_any_error(checks$round_id_valid)) {
    return(checks)
  }

  # Prerequisite
  tasks <- hubUtils::read_config(hub_path, "tasks")
  r_tasks <-
    tasks$rounds[unique(hubUtils::get_round_ids(tasks)) == round_id]
  tasks_ids <- purrr::map(purrr::map(r_tasks, "model_tasks")[[1]],
                          "task_ids")
  t_nam <- unique(unlist(purrr::map(tasks_ids, names)))

  # Partition in tasks
  test <- all(partition %in% t_nam)
  checks$partition_name <-
    capture_check_cnd(check = test, file_path = folder_path,
                      msg_subject = "Partition", error = TRUE,
                      msg_attribute = paste0("in the accepted values: `",
                                             paste(t_nam, collapse = ", "),
                                             "`."),
                      msg_verbs = c("exists", "does not exist"))

  # Prerequisite
  files_str <- strsplit(all_files, "/")

  # Partition value and format accepted
  test <- all(purrr::map_vec(files_str, ~ length(.x) == length(partition) + 1))
  checks$partition_structure <-
    capture_check_cnd(check = test, file_path = folder_path,
                      msg_subject = "Files are partitioned", error = TRUE,
                      msg_attribute = " number of columns.",
                      msg_verbs = c("in the expected",
                                    "in an unexpected"))
  if (is_any_error(checks$partition_structure) ||
        is_any_error(checks$partition_name)) {
    return(checks)
  }
  test <-
    purrr::map(seq_along(partition),
               ~ utils::URLdecode(purrr::list_transpose(files_str)[[.x]]) %in%
                 unique(unlist(purrr::map(purrr::map(r_tasks[[1]]$model_tasks,
                                                     "task_ids"),
                                          partition[.x]))))

  test <- all(unlist(test))
  checks$partition_value <-
    capture_check_cnd(check = test, file_path = folder_path,
                      msg_subject = "Partition values", error = TRUE,
                      msg_attribute = "expected.", msg_verbs = c("are",
                                                                 "are not "))
  if (is_any_error(checks$partition_value)) {
    return(checks)
  }

  # File name is correct
  checks$file_name <- try_check(check_file_name(file_name), folder_path)

  # File format
  checks$file_format <- try_check(check_file_format(file_path = file_name,
                                                    hub_path = hub_path,
                                                    round_id = round_id),
                                  folder_path)

  # Associated metadata exists
  checks$metadata_exists <-
    try_check(check_submission_metadata_file_exists(hub_path = hub_path,
                                                    file_path = file_name),
              folder_path)

  checks
}
