test_that("Test validation process", {

  lst_gs <- readRDS("training_data/2022-08-22_lst_gs.rds")
  lst_gs_flu <- NULL

  pop_path <- "https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/master/data-locations/locations.csv"
  pop_path_flu <- "https://raw.githubusercontent.com/midas-network/flu-scenario-modeling-hub/main/data-locations/locations.csv"

  js_def <- "training_data/covid_tasks.json"

  js_def_flu <- "training_data/flu_tasks.json"

  extract_err_code <- function(expr) {
    test <- capture.output(try(suppressWarnings(expr), silent = TRUE))
    code <- stringr::str_extract(
      test, "(?<=(\U0001f7e1 Warning|\U000274c Error) )\\d{3,4}")
    code <- as.character(unique(na.omit(code)))
    return(code)
  }

  ### Test on COVID ###
  testthat::expect_equal(
    validate_submission(
      "training_data/2022-01-09_no_error.csv", js_def, lst_gs, pop_path),
    "End of validation check: all the validation checks were successful")

  # Test columns error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_addcol.pqt", js_def, lst_gs, pop_path)),
    c("101", "102"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_colname.csv", js_def, lst_gs, pop_path)),
    "103")

  # Test scenario error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badidscen.csv", js_def, lst_gs, pop_path)),
    c("202", "204"))

  # Test origin_date and filename date error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_multimpd.csv", js_def, lst_gs, pop_path)),
    c("302", "303", "607"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_mpd_error.csv", js_def, lst_gs, pop_path)),
    c("004"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_mpd_format.csv", js_def, lst_gs, pop_path)),
    c("003"))

  # Test quantile error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badquant.csv", js_def, lst_gs,  pop_path)),
    c("5040", "402", "407", "406"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missquant.csv", js_def, lst_gs,  pop_path)),
    c("402", "406"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_valquant.csv", js_def, lst_gs,  pop_path)),
    c("5042", "403"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missoptquant.csv", js_def, lst_gs, pop_path)),
    character(0))

  # Test value error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badpointtype.csv", js_def, lst_gs, pop_path)),
    c("512"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badpointquant.csv", js_def, lst_gs, pop_path)),
    c("5040"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_negvalue.csv", js_def, lst_gs, pop_path)),
    c("5041"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_doublepoint.csv", js_def, lst_gs, pop_path)),
    c("510"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_doublequantzero.csv", js_def, lst_gs, pop_path)
      ), c("510"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_highvalue.csv", js_def, lst_gs, pop_path)),
    c("507"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_lowcumcase.csv", js_def, lst_gs, pop_path)),
    c("505", "508"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_lowcumdeath.csv", js_def, lst_gs, pop_path)),
    c("505", "509"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_uniquevalue.csv", js_def, lst_gs, pop_path)),
    c("505"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missingrow.csv", js_def, lst_gs, pop_path)),
    c("607", "406"))

  # Test target error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badnametarget.csv", js_def, lst_gs, pop_path)),
    c("601", "607"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_misswk.csv", js_def, lst_gs, pop_path)),
    c("605", "607"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2021-08-15_morewk.gz", js_def, lst_gs, pop_path)),
    c("606", "702"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-03-13_round13_missingweek.csv", js_def, lst_gs,
      pop_path)), c("508", "509", "605", "607"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-06-05_round14_misswk_targ.csv", js_def, lst_gs,
      pop_path)), c("508", "509", "602", "605", "607"))

  # Test location error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badlocation.csv", js_def, lst_gs, pop_path)),
    c("701", "703"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2021-11-14_no0location.zip", js_def, lst_gs, pop_path)),
    c("509", "702"))

  ### Tests on FLU ###
  testthat::expect_equal(
    validate_submission(
      "training_data/2022-08-14_flu_no_error.csv", js_def_flu, lst_gs_flu,
      pop_path_flu),
    "End of validation check: all the validation checks were successful")
  testthat::expect_equal(
    validate_submission(
      "training_data/2022-08-14_flu_nopoint_noerror.csv", js_def_flu,
      lst_gs_flu, pop_path_flu),
    "End of validation check: all the validation checks were successful")
  testthat::expect_equal(
    validate_submission(
      "training_data/2022-08-14_flu_sample.csv", js_def_flu, lst_gs_flu,
      pop_path_flu),
    "End of validation check: all the validation checks were successful")

  # Test additional location error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-08-14_flu_addloc.csv", js_def_flu, lst_gs_flu,
      pop_path_flu)), c("703"))

  # Test age-group ----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-08-14_flu_missage.csv", js_def_flu, lst_gs_flu,
      pop_path_flu)), c("802", "803","805", "804", "806"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-08-14_flu_badage.csv", js_def_flu, lst_gs_flu,
      pop_path_flu)), c("801", "804", "806"))


  # Test sample ----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-08-14_flu_badsample.csv", js_def_flu, lst_gs_flu,
      pop_path_flu)), c("901", "902", "510"))


})
