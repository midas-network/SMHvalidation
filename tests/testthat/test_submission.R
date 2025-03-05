test_that("Test validation process", {

  obs <- "../exp/target-data/time-series.csv"
  hub_path <- "../exp/"
  pop_path <- "../exp/auxiliary-data/location_census/locations.csv"
  js_def <- "../exp/hub-config/tasks.json"
  merge_sample_col <- c("run_grouping", "stochastic_run")
  partition <- round_id <- r_schema <- NULL
  n_decimal <- 1
  verbose <- TRUE

  # Files corresponding to the expected format ------
  ## Unique file -------
  path <- paste0(hub_path,
                 "model-output/team2-modelb/2023-11-12-team2-modelb.parquet")
  check <- validate_submission(path = path, js_def = js_def,
                               hub_path = hub_path, target_data = obs,
                               pop_path = pop_path,
                               merge_sample_col = merge_sample_col,
                               partition = partition, n_decimal = n_decimal,
                               round_id = round_id, verbose = verbose,
                               r_schema = r_schema)
  test <- unique(purrr::map_vec(purrr::map(check, ~attr(.x, "class")), 1))
  expect_equal(test, c("check_info", "check_success"))
  expect_equal(check$pairing_info$message,
               paste0("Run grouping pairing: \"horizon\", \"age_group\"; ",
                      "stochastic run pairing: No stochasticity. ",
                      "Number of Samples: 100"))

  ## Partition -----
  path <- paste0(hub_path,
                 "model-output/t3-mc")
  check <- validate_submission(path = path, js_def = js_def,
                               hub_path = hub_path, target_data = NULL,
                               pop_path = pop_path,
                               merge_sample_col = merge_sample_col,
                               partition = c("origin_date", "target"),
                               n_decimal = NULL,
                               round_id = round_id, verbose = verbose,
                               r_schema = r_schema)
  test <- unique(purrr::map_vec(purrr::map(check, ~attr(.x, "class")), 1))
  expect_equal(test, c("check_info", "check_success"))
  expect_equal(check$pairing_info$message,
               paste0("Run grouping pairing: \"horizon\", \"age_group\"; ",
                      "stochastic run pairing: \"horizon\", \"age_group\". ",
                      "Number of Samples: 100"))

  # Test errors --------
  path <- paste0(hub_path,
                 "model-output/team2-modelb/2023-11-12-team2-modelb.parquet")
  path_f <- paste0(hub_path,
                   "model-output/team4-modeld/2023-11-12-team4-modeld.parquet")
  df0 <- arrow::read_parquet(path)

  ## Pass - only required value ---
  df <- dplyr::filter(df0, .data[["output_type"]] == "sample") |>
    dplyr::mutate(output_type_id =
                    as.character(as.factor(paste0(.data[["run_grouping"]], "_",
                                                  .data[["stochastic_run"]]))))
  df <- dplyr::select(df, -tidyr::all_of(c("run_grouping", "stochastic_run")))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path = path_f, js_def = js_def,
                               hub_path = hub_path, target_data = NULL,
                               pop_path = pop_path,
                               merge_sample_col = NULL, partition = NULL,
                               n_decimal = NULL, round_id = round_id,
                               verbose = FALSE, r_schema = r_schema)
  expect_contains(attr(check$value_col_non_desc, "class"),
                  c("check_info", "message"))
  expect_null(check$pairing_info)

  ## Test columns error -----
  ### File with additional column ----
  df <- dplyr::mutate(df0, row = seq_along(nrow(df0)))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_equal(length(check), 5)
  expect_contains(attr(check$colnames, "class"), c("error", "check_error"))

  ### File with badly named column ----
  df <- dplyr::rename(df0, round_id = origin_date)
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
                               merge_sample_col = merge_sample_col)

  expect_equal(length(check), 1)
  expect_contains(attr(check$col_names, "class"), c("error", "check_error"))

  ## Test value in tasks id columns error -----
  ### Scenario ID error ----
  df <- dplyr::mutate(df0, scenario_id = gsub("2023", "2013",
                                              .data[["scenario_id"]]))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$valid_vals, "class"), c("error", "check_error"))

  ### Horizon for CDF output type  ----
  df <-
    dplyr::mutate(df0,
                  horizon = ifelse(.data[["output_type"]] == "cdf" &
                                     .data[["output_type_id"]] == "EW202346",
                                   1, .data[["horizon"]]),
                  horizon = as.integer(.data[["horizon"]]))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$valid_vals, "class"), c("error", "check_error"))

  ## Test origin_date and file-name date error -----
  df <- dplyr::mutate(df0,
                      origin_date = c(rep(c(as.Date("2023-11-10"),
                                            as.Date("2024-07-28")),
                                          (nrow(df0) / 2)), "2023-11-10"))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$unique_round_id, "class"),
                  c("error", "check_error"))

  ## File Name or extension error --------
  arrow::write_parquet(df0, path_f)
  file.copy(path_f, gsub("2023-11-12", "2024-07-28", path_f))
  check <- validate_submission(gsub("2023-11-12", "2024-07-28", path_f),
                               js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$match_round_id, "class"),
                  c("error", "check_error"))
  file.remove(gsub("2023-11-12", "2024-07-28", path_f))

  file.copy(path_f, gsub("2023-11-12", "2013-11-12", path_f))
  check <- validate_submission(gsub("2023-11-12", "2013-11-12", path_f),
                               js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$round_id, "class"), c("error", "check_error"))
  file.remove(gsub("2023-11-12", "2013-11-12", path_f))

  file.copy(path_f, gsub(".parquet$", ".arrow", path_f))
  check <- validate_submission(gsub(".parquet$", ".arrow", path_f),
                               js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$file_extension, "class"),
                  c("error", "check_error"))
  file.remove(gsub(".parquet$", ".arrow", path_f))

  ## File with date in unexpected format ----
  df <- dplyr::mutate(df0, origin_date = "11/12/2023")
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
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
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$rows_unique, "class"), c("error", "check_failure"))
  expect_contains(attr(check$value_col_non_desc, "class"),
                  c("error", "check_failure"))
  expect_contains(attr(check$req_vals, "class"), c("error", "check_failure"))

  ### Remove optional quantile (only 0) --------
  df <- rbind(df0,
              dplyr::filter(df0, .data[["output_type_id"]] == 0.95) |>
                dplyr::mutate(output_type_id = 1))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$req_vals, "class"), c("error", "check_failure"))

  ### Add additional horizon -------
  df <- rbind(df0,
              dplyr::filter(df0, .data[["horizon"]] == 10) |>
                dplyr::mutate(horizon = 11))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$valid_vals, "class"), c("error", "check_error"))

  ## Column Type -----------
  df <- dplyr::mutate(df0, horizon = as.double(.data[["horizon"]]))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
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
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
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
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
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
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path,
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
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path, pop_path = pop_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$population_size, "class"),
                  c("error", "check_failure"))

  ### Cumulative value lower than expected -----
  tmp_obs <- data.frame(observation = 1000, location = c("US", "06", "08"),
                        target = "cum hosp", age_group = "0-130",
                        date = as.Date("2023-11-17"))
  write.csv(tmp_obs, "../exp/target-data/tmp_ts.csv", row.names = FALSE)
  check <- validate_submission(path, js_def, hub_path, pop_path = pop_path,
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
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path, pop_path = pop_path,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$value_col_non_desc, "class"),
                  c("error", "check_failure"))
  expect_contains(attr(check$cumul_proj, "class"),
                  c("error", "check_error"))

  ### Number of decimal
  df <- dplyr::mutate(df0, value = round(.data[["value"]], 5))
  arrow::write_parquet(df, path_f)
  rm(df)
  check <- validate_submission(path_f, js_def, hub_path, n_decimal = 0,
                               merge_sample_col = merge_sample_col)
  expect_contains(attr(check$n_decimal, "class"), c("error", "check_failure"))

# # Test target error -----
# # Contains 27 week horizons instead of 13 required 26 optional (round 8)
# expect_equal(err_cd(validate_submission("tst_dt/2021-08-15_morewk.gz",
#                                         js_def, lst_gs, pop_path)),
#              c("606", "702"))
# # Contains 12 week horizons instead of 52 required (round 13))
# # JSON expect:
# #  location (for all target expect cum case): "USA" required, "unknown"
# #    optional and "US" not possible
# #  location (cum case): null
# val_test <-
#   err_cd(validate_submission("tst_dt/2022-03-13_round13_missingweek.csv",
#                              js_def, lst_gs, pop_path))
# expect_equal(val_test, c("006", "509", "5041", "508", "605", "607", "703"))
# # Cumulative cases with low value (after horizon 7)
# # JSON expect:
# #  location (for all target expect cum case): "USA" required, "unknown"
# #    optional and "US" not possible
# #  location (cum case): null
# expect_equal(err_cd(validate_submission("tst_dt/2022-03-13_round13_err.csv",
#                                         js_def, lst_gs, pop_path)),
#              c("006", "509", "5041", "508", "511", "605", "607", "703"))
# # Change inc case into inc inf target
# # JSON expect:
# #  target: inc case required inc inf optional
# val_test <-
#   err_cd(validate_submission("tst_dt/2022-06-05_round14_misswk_targ.csv",
#                              js_def, lst_gs, pop_path))
# expect_equal(val_test, c("006", "508", "509", "602", "605", "607"))

# # Test location error -----
# # Rename US as "0202" for location
# expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_badlocation.csv",
#                                         js_def, lst_gs, pop_path)),
#              c("703"))
# # Rename "02" as "2" for location
# expect_equal(err_cd(validate_submission("tst_dt/2021-11-14_no0location.zip",
#                                         js_def, lst_gs, pop_path)),
#              c("509", "702"))

# ### Tests on FLU ###
# # Test on target & horizon -----
# # remove target with "time" in the name
# val_test <- err_cd(validate_submission("tst_dt/2022-08-14_flu_misstarget.csv",
#                                        js_def_flu, lst_gs_flu, pop_path_flu))
# expect_equal(val_test, c("602"))
# # For "size" target, horizon = 1
# expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_badhorizon.csv",
#                                         js_def_flu, lst_gs_flu,
#                                         pop_path_flu)), c("606", "612"))

# # Test additional location error -----
# # add location "02" for "death" target(s)
# expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_addloc.csv",
#                                         js_def_flu, lst_gs_flu,
#                                         pop_path_flu)), c("703"))

# # Change all age_group for peak size hosp to "00_12"
# expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_badage.csv",
#                                         js_def_flu, lst_gs_flu,
#                                         pop_path_flu)),
#              c("801", "802"))
# # Remove age group column
# expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_noage.csv",
#                                         js_def_flu, lst_gs_flu,
#                                         pop_path_flu)), c("103"))

# # Test sample ----
# # Filter sample < 90 and remove inc death and peak_time target
# # for sample >= 85, rename sample into 104.5
# expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_badsample.csv",
#                                         js_def_flu, lst_gs_flu,
#                                         pop_path_flu)),
#              c("006", "204", "510", "602", "901", "903", "401"))

# # Missing all samples
# expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-quant.csv",
#                                         js_def, NULL, pop_path)),
#              c("602", "702", "904"))

# # Missing samples for one subgroup of task id
# expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-misssample.csv",
#                                         js_def, NULL, pop_path,
#                                         merge_sample_col = TRUE)),
#              c("702", "905"))

# # Mistakes in pairing ID
# expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-pair.csv",
#                                         js_def, NULL, pop_path,
#                                         merge_sample_col = TRUE)),
#              c("607", "702", "902"))

# # Double sample ID
# expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-doublesample.csv",
#                                         js_def, NULL, pop_path,
#                                         merge_sample_col = TRUE)),
#              c("510", "702"))

# # Unique sample ID
# test_val <-
#   err_cd(validate_submission("tst_dt/2024-03-26-unilettersample.csv",
#                              js_def, NULL, pop_path,
#                              merge_sample_col = TRUE))
# expect_equal(test_val, c("510", "702", "903", "902"))

# # Test value ---
# # Missing inc death (sample required) for all task_ids
# # CDF values (optional) > 1
# test_val <-
#   err_cd(validate_submission("tst_dt/2023-09-03_noincdeath_timevalue.parquet",
#                              js_def_flu, lst_gs_flu, pop_path_flu))
# expect_equal(test_val, c("006", "204", "5041", "602", "904"))

})
