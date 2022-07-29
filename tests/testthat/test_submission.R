test_that("Test validation process", {

  lst_gs <- readRDS("training_data/2022-01-09_lst_gs.rds")
  scen_info <- scen_round_info()

  extract_err_code <- function(expr) {
    test <- capture.output(try(suppressWarnings(expr), silent = TRUE))
    code <- stringr::str_extract(
      test, "(?<=(\U0001f7e1 Warning|\U000274c Error) )\\d{3}")
    code <- as.character(unique(na.omit(code)))
    return(code)
  }

  # Test no error -----
  testthat::expect_equal(
    validate_submission(
      "training_data/2022-01-09_no_error.csv", lst_gs),
    "End of validation check: all the validation checks were successfull")

  # Test prerequisite error -----
  testthat::expect_equal(
    as.character(try(suppressWarnings(validate_submission(
      "tests/testthat/training_data/2022-01-09_colname.csv", lst_gs,
      scen_info = "nofile.csv")), silent = T)),
    "Error in file(file, \"rt\") : cannot open the connection\n")
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_colname.csv", lst_gs, scen_info = 2)),
    "001")
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-15_noround.csv", lst_gs, scen_info = scen_info)),
    "002")

  # Test columns error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_addcol.pqt", lst_gs, scen_info = scen_info)),
    c("101", "102"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_colname.csv", lst_gs, scen_info = scen_info)),
    "103")

  # Test scenario error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badidscen.csv", lst_gs, scen_info = scen_info)),
    c("202", "203"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badnamescen.csv", lst_gs,
      scen_info = scen_info)), c("201", "203"))

  # Test model_projection_data and filename date error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_multimpd.csv", lst_gs, scen_info = scen_info)),
    c("302"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_mpd_error.csv", lst_gs, scen_info = scen_info)),
    c("303", "304"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2922-01-09_mpdfile_error.csv", lst_gs,
      scen_info = scen_info)), c("304"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_mpd_format.csv", lst_gs,
      scen_info = scen_info)), c("301", "303", "304"))

  # Test quantile error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badquant.csv", lst_gs, scen_info = scen_info)),
    c("401"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missquant.csv", lst_gs, scen_info = scen_info)),
    c("402"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_valquant.csv", lst_gs, scen_info = scen_info)),
    c("403", "504"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missoptquant.csv", lst_gs,
      scen_info = scen_info)), character(0))

  # Test value error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badpointtype.csv", lst_gs,
      scen_info = scen_info)), c("404", "501", "503", "506"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badpointquant.csv", lst_gs,
      scen_info = scen_info)), c("502", "510"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_negvalue.csv", lst_gs,
      scen_info = scen_info)), c("504"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_doublepoint.csv", lst_gs,
      scen_info = scen_info)), c("404", "503", "506", "510"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_doublequantzero.csv", lst_gs,
      scen_info = scen_info)), c("404", "510"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_highvalue.csv", lst_gs, scen_info = scen_info)),
    c("504", "507"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_lowcumcase.csv", lst_gs,
      scen_info = scen_info)), c("508"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_lowcumdeath.csv", lst_gs,
      scen_info = scen_info)), c("509"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_uniquevalue.csv", lst_gs,
      scen_info = scen_info)),
    c("505"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_missingrow.csv", lst_gs,
      scen_info = scen_info)),
    c("510", "607"))

  # Test target error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badnametarget.csv", lst_gs,
      scen_info = scen_info)), c("601", "607"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_misswk.csv", lst_gs, scen_info = scen_info)),
    c("604"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2021-08-15_morewk.gz", lst_gs,
      scen_info = scen_info)), c("507", "606", "702"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badstartdate.csv", lst_gs,
      scen_info = scen_info)), c("603", "608", "609"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-03-13_round13_missingweek.csv", lst_gs,
      scen_info = scen_info)), c("508", "509", "605"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-06-05_round14_misswk_targ.csv", lst_gs,
      scen_info = scen_info)), c("508", "509", "602", "605"))

  # Test location error -----
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2022-01-09_badlocation.csv", lst_gs,
      scen_info = scen_info)), c("701"))
  testthat::expect_equal(
    extract_err_code(validate_submission(
      "training_data/2021-11-14_no0location.zip", lst_gs,
      scen_info = scen_info)), c("507", "702"))

})
