test_that("Test validation process", {

  obs <- "example_hub/target-data/time-series.csv"
  hub_path <- "example_hub/"
  pop_path <- "example_hub/auxiliary-data/location_census/locations.csv"
  js_def <- "example_hub/hub-config/tasks.json"
  merge_sample_col <- c("run_grouping", "stochastic_run")
  partition <- round_id <- r_schema <- NULL
  n_decimal <- 1
  verbose <- TRUE


  ### Test on COVID ###
  # File corresponding to the expected format
  path <- paste0(hub_path,
                 "model-output/team2-modelb/2023-11-12-team2-modelb.parquet")
  validate_submission(path = path, js_def = js_def, hub_path = hub_path,
                      target_data = obs, pop_path = pop_path,
                      merge_sample_col = merge_sample_col,
                      partition = partition, n_decimal = n_decimal,
                      round_id = round_id, verbose = verbose,
                      r_schema = r_schema)

  expect_equal(validate_submission("tst_dt/2024-03-26-team1-modela.parquet",
                                   js_def, NULL, pop_path,
                                   merge_sample_col = TRUE), NULL)

  # Test columns error -----
  # File with additional column ("row": row number id)
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_addcol.pqt",
                                          js_def, lst_gs, pop_path)),
               c("104", "102"))

  expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-badcol.csv",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE)), "101")

  # Partition test
  expect_equal(err_cd(validate_submission("tst_dt/partition_ok/",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE,
                                          partition = "target",
                                          round_id = "2024-03-26")),
               character(0))
  expect_equal(err_cd(validate_submission("tst_dt/partition_ok2/",
                                          js_def, NULL, pop_path,
                                          partition = "target")),
               character(0))
  expect_equal(err_cd(validate_submission("tst_dt/partition_format/",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE,
                                          partition = "target",
                                          round_id = "2024-03-26")), "005")
  expect_equal(err_cd(validate_submission("tst_dt/partition_err/",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE,
                                          partition = "target",
                                          round_id = "2024-03-26")), "101")


  # File with "location" column renamed "fips"
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_colname.csv",
                                          js_def, lst_gs, pop_path)), "103")

  # Test scenario error -----
  # File with scenario name incorrect: A-2020 instead of A-2022
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_badidscen.csv",
                                          js_def, lst_gs, pop_path)),
               c("006", "202", "204"))

  # Test origin_date and filename date error -----
  # File with 2 values in the origin date column (1 expected and 1 not expected)
  # half of all the row with incorrect value
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_multimpd.csv",
                                          js_def, lst_gs, pop_path)),
               c("006", "302", "303", "607"))
  # File with incorrect origin date column value (unique value: 2922 instead of
  # 2022) and filename
  expect_equal(err_cd(validate_submission("tst_dt/2922-01-09_mpd_error.csv",
                                          js_def, lst_gs, pop_path)), c("004"))
  # File with incorrect origin date column format (DD/MM/YYYY instead of
  # YYYY-MM-DD)
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_mpd_format.csv",
                                          js_def, lst_gs, pop_path)), c("003"))

  # File with date in POSIX format
  expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-format.parquet",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE)),
               c("305", "702"))

  # Test quantile error -----
  # File with incorrect quantile: replace 0.01 by 0.02 (unexpected value)
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_badquant.csv",
                                          js_def, lst_gs,  pop_path)),
               c("5040", "402", "407", "406"))
  # File with missing required quantile: remove all 0.5 quantile
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_missquant.csv",
                                          js_def, lst_gs,  pop_path)),
               c("402", "406"))
  # File with unique value for all US, Scenario A, quantile 1, inc death (= 0)
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_valquant.csv",
                                          js_def, lst_gs,  pop_path)),
               c("5042", "403"))
  # File with missing optional quantile: remove all 0 and 1 quantile
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_missoptquant.csv",
                                          js_def, lst_gs, pop_path)),
               character(0))

  # Test value error -----
  # Rename type column with NA type_id value into "pnt" (instead of median)
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_badpointtype.csv",
                                          js_def, lst_gs, pop_path)), c("512"))
  # Associated 0.5 instead of NA in the type_id column for median type value
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_badpointquant.csv",
                                          js_def, lst_gs, pop_path)), c("5040"))
  # negative value of quantile 0, Scenario A, location US, inc death
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_negvalue.csv",
                                          js_def, lst_gs, pop_path)), c("5041"))
  # Duplicates value for Scenario A location US, inc death
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_doublepoint.csv",
                                          js_def, lst_gs, pop_path)), c("510"))
  # Duplicates value for Scenario A location US, inc death, quantile 0
  # (optional)
  val_test <-
    err_cd(validate_submission("tst_dt/2022-01-09_doublequantzero.csv",
                               js_def, lst_gs, pop_path))
  expect_equal(val_test, c("510"))
  # High value (1e9) of quantile 1, Scenario A, location US, inc death
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_highvalue.csv",
                                          js_def, lst_gs, pop_path)), c("507"))
  expect_equal(validate_submission("tst_dt/2022-01-09_highvalue.csv", js_def),
               NULL)
  # Low value (10) of Scenario A, location US, cum case
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_lowcumcase.csv",
                                          js_def, lst_gs, pop_path)),
               c("505", "508"))
  # Low value (1) of Scenario A, location US, cum death
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_lowcumdeath.csv",
                                          js_def, lst_gs, pop_path)),
               c("505", "509"))
  #  Unique value (1) of Scenario A, location US, inc death
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_uniquevalue.csv",
                                          js_def, lst_gs, pop_path)), c("505"))
  # File with missing first 100 rows
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_missingrow.csv",
                                          js_def, lst_gs, pop_path)),
               c("607", "406"))

  # Test target error -----
  # Rename "inc case" into "inccase"
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_badnametarget.csv",
                                          js_def, lst_gs, pop_path)),
               c("006", "601", "602"))
  # Remove horizon == 12 rows
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_misswk.csv",
                                          js_def, lst_gs, pop_path)),
               c("006", "605", "607"))
  # Contains 27 week horizons instead of 13 required 26 optional (round 8)
  expect_equal(err_cd(validate_submission("tst_dt/2021-08-15_morewk.gz",
                                          js_def, lst_gs, pop_path)),
               c("606", "702"))
  # Contains 12 week horizons instead of 52 required (round 13))
  # JSON expect:
  #  location (for all target expect cum case): "USA" required, "unknown"
  #    optional and "US" not possible
  #  location (cum case): null
  val_test <-
    err_cd(validate_submission("tst_dt/2022-03-13_round13_missingweek.csv",
                               js_def, lst_gs, pop_path))
  expect_equal(val_test, c("006", "509", "5041", "508", "605", "607", "703"))
  # Cumulative cases with low value (after horizon 7)
  # JSON expect:
  #  location (for all target expect cum case): "USA" required, "unknown"
  #    optional and "US" not possible
  #  location (cum case): null
  expect_equal(err_cd(validate_submission("tst_dt/2022-03-13_round13_err.csv",
                                          js_def, lst_gs, pop_path)),
               c("006", "509", "5041", "508", "511", "605", "607", "703"))
  # Change inc case into inc inf target
  # JSON expect:
  #  target: inc case required inc inf optional
  val_test <-
    err_cd(validate_submission("tst_dt/2022-06-05_round14_misswk_targ.csv",
                               js_def, lst_gs, pop_path))
  expect_equal(val_test, c("006", "508", "509", "602", "605", "607"))

  # Test location error -----
  # Rename US as "0202" for location
  expect_equal(err_cd(validate_submission("tst_dt/2022-01-09_badlocation.csv",
                                          js_def, lst_gs, pop_path)),
               c("703"))
  # Rename "02" as "2" for location
  expect_equal(err_cd(validate_submission("tst_dt/2021-11-14_no0location.zip",
                                          js_def, lst_gs, pop_path)),
               c("509", "702"))

  ### Tests on FLU ###
  # No error (contains optional point value)
  expect_equal(validate_submission("tst_dt/2022-08-14_flu_no_error.csv",
                                   js_def_flu, lst_gs_flu, pop_path_flu), NULL)
  # No error (missing optional point value)
  expect_equal(validate_submission("tst_dt/2022-08-14_flu_nopoint_noerror.csv",
                                   js_def_flu, lst_gs_flu, pop_path_flu), NULL)
  # No error (contains quantile and optional sample format)
  expect_equal(validate_submission("tst_dt/2022-08-14_flu_sample.csv",
                                   js_def_flu, lst_gs_flu, pop_path_flu), NULL)
  # Test on target & horizon -----
  # remove target with "time" in the name
  val_test <- err_cd(validate_submission("tst_dt/2022-08-14_flu_misstarget.csv",
                                         js_def_flu, lst_gs_flu, pop_path_flu))
  expect_equal(val_test, c("602"))
  # For "size" target, horizon = 1
  expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_badhorizon.csv",
                                          js_def_flu, lst_gs_flu,
                                          pop_path_flu)), c("606", "612"))

  # Test value sample
  # No error (contains quantile and optional sample format)
  expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_sample.csv",
                                          js_def_flu, lst_gs_flu, pop_path_flu,
                                          n_decimal = 1)), c("5043"))

  # Test additional location error -----
  # add location "02" for "death" target(s)
  expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_addloc.csv",
                                          js_def_flu, lst_gs_flu,
                                          pop_path_flu)), c("703"))

  # Test age-group ----
  # Change all age_group to "130-17"
  expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_missage.csv",
                                          js_def_flu, lst_gs_flu,
                                          pop_path_flu)),
               c("006", "802"))
  # Change all age_group for peak size hosp to "00_12"
  expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_badage.csv",
                                          js_def_flu, lst_gs_flu,
                                          pop_path_flu)),
               c("801", "802"))
  # Remove age group column
  expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_noage.csv",
                                          js_def_flu, lst_gs_flu,
                                          pop_path_flu)), c("103"))

  # Test sample ----
  # Filter sample < 90 and remove inc death and peak_time target
  # for sample >= 85, rename sample into 104.5
  expect_equal(err_cd(validate_submission("tst_dt/2022-08-14_flu_badsample.csv",
                                          js_def_flu, lst_gs_flu,
                                          pop_path_flu)),
               c("006", "204", "510", "602", "901", "903", "401"))

  # Missing all samples
  expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-quant.csv",
                                          js_def, NULL, pop_path)),
               c("602", "702", "904"))

  # Missing samples for one subgroup of task id
  expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-misssample.csv",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE)),
               c("702", "905"))

  # Mistakes in pairing ID
  expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-pair.csv",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE)),
               c("607", "702", "902"))

  # Double sample ID
  expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-doublesample.csv",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE)),
               c("510", "702"))

  # Unique sample ID
  test_val <-
    err_cd(validate_submission("tst_dt/2024-03-26-unilettersample.csv",
                               js_def, NULL, pop_path,
                               merge_sample_col = TRUE))
  expect_equal(test_val, c("510", "702", "903", "902"))

  # Test value ---
  # Missing inc death (sample required) for all task_ids
  # CDF values (optional) > 1
  test_val <-
    err_cd(validate_submission("tst_dt/2023-09-03_noincdeath_timevalue.parquet",
                               js_def_flu, lst_gs_flu, pop_path_flu))
  expect_equal(test_val, c("006", "204", "5041", "602", "904"))

  # Missing inc death (sample required) for all task_ids
  # Only sample as output_type
  test_val <-
    err_cd(validate_submission("tst_dt/2023-09-03_noincdeath_sample.parquet",
                               js_def_flu, lst_gs_flu, pop_path_flu))
  expect_equal(test_val, c("006", "204", "602", "904"))

  # Contains both parquet files:
  # Missing inc death (sample required) for all task_ids
  # CDF values (optional) > 1
  # double sample inc hosp
  expect_equal(err_cd(validate_submission(dir("tst_dt/", pattern = "2023-09-03",
                                              full.names = TRUE),
                                          js_def_flu, lst_gs_flu,
                                          pop_path_flu, verbose = FALSE)),
               c("006", "204", "510", "5041", "602", "901", "904"))

  # Test race/ethnicity -----
  expect_equal(err_cd(validate_submission("tst_dt/2024-03-26-raceethn.csv",
                                          js_def, NULL, pop_path,
                                          merge_sample_col = TRUE)),
               c("006", "702", "902", "1002", "1001"))

  ### Internal Functions ###
  df <- read.csv("tst_dt/2022-08-14_flu_no_error.csv")
  js <- jsonlite::read_json("tst_dt/flu_tasks.json")
  js_tasks <- js$rounds[[1]]$model_tasks
  df_test <- SMHvalidation:::filter_df(df, js_tasks)
  expect_equal(nrow(dplyr::setdiff(df, df_test)), 0)

  ### Error files ####
  # Bad link
  test_val <-
    err_cd(validate_submission("tst_dt/2023-09-03_noincdeath_sample.txt",
                               js_def_flu, lst_gs_flu, pop_path_flu))
  expect_equal(test_val, c("005"))
})
