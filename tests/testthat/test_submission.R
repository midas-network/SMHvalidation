test_that("Test validation process", {

  obs <- "../exp/target-data/time-series.csv"
  hub_path <- "../exp/"
  pop_path <- "../exp/auxiliary-data/location_census/locations.csv"
  merge_sample_col <- c("run_grouping", "stochastic_run")
  partition <- round_id <- r_schema <- NULL
  n_decimal <- 1
  verbose <- TRUE
  path <- "team2-modelb/2023-11-12-team2-modelb.parquet"
  path_f <- "team4-modeld/2023-11-12-team4-modeld.parquet"
  path_f_write <-
    paste0(hub_path,
           "model-output/team4-modeld/2023-11-12-team4-modeld.parquet")
  df0 <- arrow::read_parquet(paste0(hub_path, "model-output/", path))

  # Files corresponding to the expected format ------
  ## Partition -----
  check <- validate_submission(path = "t3-mc", hub_path = hub_path,
                               target_data = NULL, pop_path = pop_path,
                               merge_sample_col = merge_sample_col,
                               partition = c("origin_date", "target"),
                               n_decimal = NULL,
                               round_id = round_id, verbose = verbose,
                               r_schema = r_schema)
  test <- unique(purrr::map_vec(purrr::map(check, ~attr(.x, "class")), 1))
  expect_equal(test, c("check_info", "check_success"))
  expect_equal(check$pairing_info$message,
               paste0("Run grouping pairing: horizon, age_group; stochastic ",
                      "run pairing: horizon, age_group. ",
                      "Number of Samples: 100"))

  ## Unique file -------
  check <- validate_submission(path = path, hub_path = hub_path,
                               target_data = obs, pop_path = pop_path,
                               merge_sample_col = merge_sample_col,
                               partition = partition, n_decimal = n_decimal,
                               round_id = round_id, verbose = verbose,
                               r_schema = r_schema, verbose_col = "age_group")
  test <- unique(purrr::map_vec(purrr::map(check, ~attr(.x, "class")), 1))
  expect_equal(test, c("check_info", "check_success"))
  expect_equal(check$pairing_info$message,
               paste0("Run grouping pairing: horizon, age_group (0-130, ",
                      "65-130); No stochasticity. ",
                      "Number of Samples: 100"))

  ## only required value ---
  df <- dplyr::filter(df0, .data[["output_type"]] == "sample") |>
    dplyr::mutate(output_type_id =
                    as.character(as.factor(paste0(.data[["run_grouping"]], "_",
                                                  .data[["stochastic_run"]]))))
  df <- dplyr::select(df, -tidyr::all_of(c("run_grouping", "stochastic_run")))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path = path_f, hub_path = hub_path,
                               target_data = NULL, pop_path = pop_path,
                               merge_sample_col = NULL, partition = NULL,
                               n_decimal = NULL, round_id = round_id,
                               verbose = FALSE, r_schema = r_schema)
  expect_contains(attr(check$value_col_non_desc, "class"),
                  c("check_info", "message"))
  expect_null(check$pairing_info)

  ## Exchange pairing information ---
  df <- df0
  df$run_grouping <- df0$stochastic_run
  df$stochastic_run <- df0$run_grouping
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path = path_f, hub_path = hub_path,
                               target_data = obs, pop_path = pop_path,
                               merge_sample_col = merge_sample_col,
                               partition = partition, n_decimal = n_decimal,
                               round_id = round_id, verbose = verbose,
                               r_schema = r_schema)
  test <- unique(purrr::map_vec(purrr::map(check, ~attr(.x, "class")), 1))
  expect_equal(test, c("check_info", "check_success"))
  expect_equal(check$pairing_info$message,
               paste0("No run grouping pairing; stochastic run pairing: ",
                      "horizon, age_group. Number of Samples: 100"))

  # Test errors --------

  ## Missing required value ------
  df <- dplyr::filter(df0, .data[["target"]] == "cum hosp") |>
    dplyr::select(-tidyr::all_of(c("run_grouping", "stochastic_run")))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path)
  expect_contains(attr(check$req_vals, "class"), c("error", "check_error"))

  ## Additional data
  df <- rbind(df0,
              dplyr::filter(df0, .data[["horizon"]] == 10) |>
                dplyr::mutate(value = .data[["value"]] + 1,
                              horizon = 11))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$valid_vals, "class"), c("error", "check_error"))

  ## Test columns error -----
  ### File with additional column ----
  df <- dplyr::mutate(df0, row = seq_along(nrow(df0)))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_equal(length(check), 5)
  expect_contains(attr(check$colnames, "class"), c("error", "check_error"))

  ### File with badly named column ----
  df <- dplyr::rename(df0, round_id = origin_date)
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)

  expect_equal(length(check), 1)
  expect_contains(attr(check$col_names, "class"), c("error", "check_error"))

  ## Test value in tasks id columns error -----
  ### Scenario ID error ----
  df <- dplyr::mutate(df0, scenario_id = gsub("2023", "2013",
                                              .data[["scenario_id"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$valid_vals, "class"), c("error", "check_error"))

  ### Horizon for CDF output type  ----
  df <-
    dplyr::mutate(df0,
                  horizon = ifelse(.data[["output_type"]] == "cdf" &
                                     .data[["output_type_id"]] == "EW202346",
                                   1, .data[["horizon"]]),
                  horizon = as.integer(.data[["horizon"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$valid_vals, "class"), c("error", "check_error"))

  ## Test origin_date and file-name date error -----
  df <- dplyr::mutate(df0,
                      origin_date = c(rep(c(as.Date("2023-11-10"),
                                            as.Date("2024-07-28")),
                                          (nrow(df0) / 2)), "2023-11-10"))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$unique_round_id, "class"),
                  c("error", "check_error"))

  ## File Name or extension error --------
  arrow::write_parquet(df0, path_f_write)
  file.copy(path_f_write, gsub("2023-11-12", "2024-07-28", path_f_write))
  check <- validate_submission(gsub("2023-11-12", "2024-07-28", path_f),
                               hub_path, merge_sample_col = merge_sample_col)
  expect_contains(attr(check$match_round_id, "class"),
                  c("error", "check_error"))
  file.remove(gsub("2023-11-12", "2024-07-28", path_f_write))

  file.copy(path_f_write, gsub("2023-11-12", "2013-11-12", path_f_write))
  check <- validate_submission(gsub("2023-11-12", "2013-11-12", path_f),
                               hub_path, merge_sample_col = merge_sample_col)
  expect_contains(attr(check$round_id, "class"), c("error", "check_error"))
  file.remove(gsub("2023-11-12", "2013-11-12", path_f_write))

  file.copy(path_f_write, gsub(".parquet$", ".arrow", path_f_write))
  check <- validate_submission(gsub(".parquet$", ".arrow", path_f),
                               hub_path, merge_sample_col = merge_sample_col)
  expect_contains(attr(check$file_extension, "class"),
                  c("error", "check_error"))
  file.remove(gsub(".parquet$", ".arrow", path_f_write))

  ## File with date in unexpected format ----
  df <- dplyr::mutate(df0, origin_date = "11/12/2023")
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$date_format, "class"),
                  c("error", "check_error"))

  ## File with wrong output ------
  ### Quantile --------
  df <-
    dplyr::mutate(df0,
                  output_type_id = ifelse(.data[["output_type"]] == "quantile" &
                                            .data[["output_type_id"]] == 0.5,
                                          0.2, .data[["output_type_id"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$rows_unique, "class"), c("error", "check_failure"))
  expect_contains(attr(check$req_vals, "class"), c("error", "check_failure"))
  expect_contains(attr(check$value_col_non_desc, "class"),
                  c("error", "check_failure"))

  ### Remove optional quantile (only 0) --------
  df <- rbind(df0,
              dplyr::filter(df0, .data[["output_type_id"]] == 0.95) |>
                dplyr::mutate(output_type_id = 1))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$req_vals, "class"), c("error", "check_failure"))

  ### Add additional horizon -------
  df <- rbind(df0,
              dplyr::filter(df0, .data[["horizon"]] == 10) |>
                dplyr::mutate(horizon = 11))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$valid_vals, "class"), c("error", "check_error"))

  ## Column Type -----------
  df <- dplyr::mutate(df0, horizon = as.double(.data[["horizon"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$col_types, "class"), c("error", "check_failure"))

  ## Error in the value column ------
  ### Flat trajectory ---------
  df <-
    dplyr::mutate(df0, value = ifelse(.data[["scenario_id"]] == "A-2023-10-27" &
                                        .data[["age_group"]] == "0-130" &
                                        .data[["target"]] == "inc hosp" &
                                        .data[["location"]] == "US" &
                                        .data[["output_type"]] == "sample" &
                                        .data[["run_grouping"]] %in% c(1:10),
                                      0, .data[["value"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$flat_projection, "class"),
                  c("error", "check_failure"))

  ### NA value -----------
  df <-
    dplyr::mutate(df0, value = ifelse(.data[["scenario_id"]] == "A-2023-10-27" &
                                        .data[["age_group"]] == "0-130" &
                                        .data[["target"]] == "inc hosp" &
                                        .data[["location"]] == "US" &
                                        .data[["output_type"]] == "sample" &
                                        .data[["run_grouping"]] %in% c(1:10) &
                                        .data[["horizon"]] == 10,
                                      NA, .data[["value"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$value_col_valid, "class"),
                  c("error", "check_failure"))
  expect_contains(attr(check$na_value, "class"), c("error", "check_error"))

  ### Negative value -----------
  df <-
    dplyr::mutate(df0, value = ifelse(.data[["scenario_id"]] == "A-2023-10-27" &
                                        .data[["age_group"]] == "0-130" &
                                        .data[["target"]] == "inc hosp" &
                                        .data[["location"]] == "US" &
                                        .data[["output_type"]] == "sample" &
                                        .data[["run_grouping"]] == 1 &
                                        .data[["horizon"]] == 1,
                                      -1, .data[["value"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$value_col_valid, "class"),
                  c("error", "check_failure"))

  ### Greater than population size -----------
  df <-
    dplyr::mutate(df0, value = ifelse(.data[["scenario_id"]] == "A-2023-10-27" &
                                        .data[["age_group"]] == "0-130" &
                                        .data[["target"]] == "inc hosp" &
                                        .data[["location"]] == "US" &
                                        .data[["output_type"]] == "sample" &
                                        .data[["run_grouping"]] == 1 &
                                        .data[["horizon"]] == 1,
                                      7e9, .data[["value"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path, pop_path = pop_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$population_size, "class"),
                  c("error", "check_failure"))

  ### Cumulative value lower than expected -----
  tmp_obs <- data.frame(observation = 1000, location = c("US", "06", "08"),
                        target = "cum hosp", age_group = "0-130",
                        date = as.Date("2023-11-17"))
  write.csv(tmp_obs, "../exp/target-data/tmp_ts.csv", row.names = FALSE)
  rm(tmp_obs)
  check <- validate_submission(path, hub_path, pop_path = pop_path,
                               target_data =  "../exp/target-data/tmp_ts.csv",
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$cumul_value, "class"), c("error", "check_failure"))
  file.remove("../exp/target-data/tmp_ts.csv")

  ### Cumulative value decreasing -----
  df <-
    dplyr::mutate(df0, value = ifelse(.data[["scenario_id"]] == "A-2023-10-27" &
                                        .data[["age_group"]] == "0-130" &
                                        .data[["target"]] == "cum hosp" &
                                        .data[["location"]] == "US" &
                                        .data[["output_type"]] == "quantile" &
                                        .data[["output_type_id"]] == 0.5 &
                                        .data[["horizon"]] == 10,
                                      1, .data[["value"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path, pop_path = pop_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$value_col_non_desc, "class"),
                  c("error", "check_failure"))
  expect_contains(attr(check$cumul_proj, "class"),
                  c("error", "check_error"))

  ### Number of decimal
  df <- dplyr::mutate(df0, value = round(.data[["value"]], 5))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- validate_submission(path_f, hub_path, n_decimal = 0,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$n_decimal, "class"), c("error", "check_failure"))

  ## Read file error ---------
  ### Location ---------
  df <- dplyr::mutate(df0, location = gsub("0", "", .data[["location"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  quiet_log <- purrr:::quietly(validate_submission)
  check <- quiet_log(path_f, hub_path, merge_sample_col = merge_sample_col)
  expect_equal(check$warnings,
               paste0("Some location value are missing a trailing 0. ",
                      "For example, 6, 8 instead of 06, 08"))
  expect_equal(unique(purrr::map_vec(purrr::map(check$result,
                                                ~attr(.x, "class")), 1)),
               c("check_info", "check_success"))

  ### Factor -----------
  df <- dplyr::mutate(df0, age_group = as.factor(.data[["age_group"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  quiet_log <- purrr:::quietly(validate_submission)
  check <- quiet_log(path_f, hub_path, merge_sample_col = merge_sample_col)
  expect_contains(check$warnings,
                  paste0("🟡 Warning: At least one column is in a format: ",
                         "'factor', please verify. \n The column(s) will be ",
                         "automatically set to  'character'."))
  expect_equal(unique(purrr::map_vec(purrr::map(check$result,
                                                ~attr(.x, "class")), 1)),
               c("check_info", "check_success"))

  ### Sample in unaccepted format -------
  df <- dplyr::mutate(df0, stochastic_run = 1.25)
  arrow::write_parquet(df, path_f_write)
  rm(df)
  quiet_log <- purrr:::quietly(validate_submission)
  check <- quiet_log(path_f, hub_path, verbose = FALSE,
                     merge_sample_col = merge_sample_col)
  expect_contains(check$messages,
                  paste0("❌ Error: The columns run_grouping and ",
                         "stochastic_run should contain integer values ",
                         "only for type 'sample'.\n"))
  expect_contains(unique(purrr::map_vec(purrr::map(check$result,
                                                   ~attr(.x, "class")), 1)),
                  c("check_success", "check_info"))

  ### Add an schema -------
  js_def0 <- hubUtils::read_config_file("../exp/hub-config/tasks.json")
  js_def2 <- hubUtils::get_round_model_tasks(js_def0, "2023-11-12")
  schema <- SMHvalidation:::make_schema(js_def0, js_def2, "2023-11-12")
  check <- try(quiet_log("t3-mc", hub_path, verbose = FALSE, r_schema = schema,
                         merge_sample_col = merge_sample_col,
                         round_id = "2023-11-12",
                         partition = c("origin_date", "target")))
  expect_equal(class(check), "try-error")
  expect_contains(attr(check, "condition")$message,
                  paste0("❌ Error: At least one column name is misspelled",
                         " or does not correspond to the expected column names",
                         " \n"))

  ## Sample -------
  ### All samples have the same group ID -------
  df <- dplyr::mutate(df0, run_grouping = 1)
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- try(quiet_log(path_f, hub_path, merge_sample_col = merge_sample_col))
  expect_contains(check$messages,
                  paste0("\n❌ Error: The submission should contains multiple",
                         " sample output type groups, please verify.\n\n"))
  expect_contains(attr(check$result$rows_unique, "class"),
                  c("error", "check_failure"))
  expect_contains(attr(check$result$spl_compound_tid, "class"),
                  c("error", "check_error"))

  ### Pairing information incorrect - ID Set -------
  df <- dplyr::mutate(df0, run_grouping = dplyr::cur_group_id(),
                      .by = tidyr::all_of(c("scenario_id", "target", "location",
                                            "age_group", "output_type")))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- try(quiet_log(path_f, hub_path,
                         merge_sample_col = merge_sample_col))
  expect_contains(attr(check$result$spl_compound_taskid_set, "class"),
                  c("error", "check_error"))

  ### Pairing information incorrect - Specific Group -------
  df <-
    dplyr::mutate(df0, location = ifelse(.data[["run_grouping"]] == 1 &
                                           .data[["output_type"]] == "sample",
                                         "06", .data[["location"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- try(quiet_log(path_f, hub_path,
                         merge_sample_col = merge_sample_col))
  expect_contains(attr(check$result$spl_n, "class"),
                  c("error", "check_error"))

  ### Pairing information incorrect - Re-ID one sample -------
  df <-
    dplyr::mutate(df0, run_grouping = ifelse(.data[["scenario_id"]] ==
                                               "A-2023-10-27" &
                                               .data[["age_group"]] == "0-130" &
                                               .data[["target"]] == "inc hosp" &
                                               .data[["location"]] == "US" &
                                               .data[["output_type"]] ==
                                                 "sample" &
                                               .data[["run_grouping"]] == 1 &
                                               .data[["horizon"]] == 1,
                                             2, .data[["run_grouping"]]))
  arrow::write_parquet(df, path_f_write)
  rm(df)
  check <- try(quiet_log(path_f, hub_path,
                         merge_sample_col = merge_sample_col))
  expect_contains(attr(check$result$spl_compound_tid, "class"),
                  c("error", "check_error"))
})
