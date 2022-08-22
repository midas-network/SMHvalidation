test_that("Test validation process", {

  lst_gs <- readRDS("training_data/2022-08-22_lst_gs.rds")
  pop_path <- "https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/master/data-locations/locations.csv"
  js_def <- "training_data/2022-01-09_metadata.json"
  js_def <- jsonlite::fromJSON(js_def)

  extract_err_code <- function(expr) {
    test <- capture.output(try(suppressWarnings(expr), silent = TRUE))
    code <- stringr::str_extract(
      test, "(?<=(\U0001f7e1 Warning|\U000274c Error) )\\d{3,4}")
    code <- as.character(unique(na.omit(code)))
    return(code)
  }

  # Test no error -----
  testthat::expect_equal(
    validate_submission(
      "training_data/2022-01-09_no_error.csv", lst_gs, pop_path, js_def),
    "End of validation check: all the validation checks were successfull")

  # Test columns error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_addcol.pqt", lst_gs, pop_path, js_def)),
    c("101", "102"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_colname.csv", lst_gs, pop_path, js_def)),
    "103")

  # Test scenario error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badidscen.csv", lst_gs, pop_path, js_def)),
    c("202", "203"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badnamescen.csv", lst_gs, pop_path, js_def)),
    c("201", "203"))

  # Test model_projection_data and filename date error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_multimpd.csv", lst_gs, pop_path, js_def)),
    c("302"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_mpd_error.csv", lst_gs, pop_path, js_def)),
    c("303", "304"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2922-01-09_mpdfile_error.csv", lst_gs, pop_path, js_def)),
    c("304"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_mpd_format.csv", lst_gs, pop_path, js_def)),
    c("301", "303", "304"))

  # Test quantile error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badquant.csv", lst_gs,  pop_path, js_def)),
    c("401"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missquant.csv", lst_gs,  pop_path, js_def)),
    c("402"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_valquant.csv", lst_gs,  pop_path, js_def)),
    c("403", "5041", "5042"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missoptquant.csv", lst_gs, pop_path, js_def)),
    character(0))

  # Test value error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badpointtype.csv", lst_gs, pop_path, js_def)),
    c("501", "503", "506"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badpointquant.csv", lst_gs, pop_path, js_def)),
    c("502", "510"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_negvalue.csv", lst_gs, pop_path, js_def)),
    c("5041"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_doublepoint.csv", lst_gs, pop_path, js_def)),
    c("404", "503", "506", "510"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_doublequantzero.csv", lst_gs, pop_path,
      js_def)), c("404", "510"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_highvalue.csv", lst_gs, pop_path, js_def)),
    c("507"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_lowcumcase.csv", lst_gs, pop_path, js_def)),
    c("405", "508"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_lowcumdeath.csv", lst_gs, pop_path, js_def)),
    c("405", "509"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_uniquevalue.csv", lst_gs, pop_path, js_def)),
    c("405", "505"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missingrow.csv", lst_gs, pop_path, js_def)),
    c("607"))

  # Test target error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badnametarget.csv", lst_gs, pop_path, js_def)),
    c("601", "607"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_misswk.csv", lst_gs, pop_path, js_def)),
    c("605", "607"))
#  testthat::expect_equal(
#    extract_err_code(validate_submission(
#      "training_data/2021-08-15_morewk.gz", lst_gs,
#      scen_info = scen_info)), c("507", "606", "702"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badstartdate.csv", lst_gs, pop_path, js_def)),
    c("603", "608", "609"))
#  testthat::expect_equal(
#    extract_err_code(validate_submission(
#      "training_data/2022-03-13_round13_missingweek.csv", lst_gs,
#      scen_info = scen_info)), c("508", "509", "605"))
#  testthat::expect_equal(
#    extract_err_code(validate_submission(
#      "training_data/2022-06-05_round14_misswk_targ.csv", lst_gs,
#      scen_info = scen_info)), c("508", "509", "602", "605"))

  # Test location error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badlocation.csv", lst_gs, pop_path, js_def)),
    c("701"))
#  testthat::expect_equal(
#    extract_err_code(validate_submission(
#      "training_data/2021-11-14_no0location.zip", lst_gs,
#      scen_info = scen_info)), c("507", "702"))

})
