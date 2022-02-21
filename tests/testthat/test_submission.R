test_that("Test validation process", {

  lst_gs <- readRDS("training_data/2022-01-09_lst_gs.rds")
  scen_info <- scen_round_info()

  testthat::expect_equal(
    validate_submission(
      "training_data/2022-01-09_no_error.csv", lst_gs, scen_info = scen_info),
    "End of validation check: all the validation checks were successfull")

  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_colname.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_addcol.csv", lst_gs, scen_info = scen_info))

  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_badidscen.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_badnamescen.csv", lst_gs, scen_info = scen_info))

  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_multimpd.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_mpd_error.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2922-01-09_mpdfile_error.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_mpd_format.csv", lst_gs, scen_info = scen_info))

  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_badquant.csv", lst_gs, scen_info = scen_info))
  testthat::expect_warning(validate_submission(
    "training_data/2022-01-09_missquant.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_valquant.csv", lst_gs, scen_info = scen_info))
  testthat::expect_equal(
    validate_submission(
      "training_data/2022-01-09_missoptquant.csv", lst_gs,
      scen_info = scen_info),
    "End of validation check: all the validation checks were successfull")

  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_badpointtype.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_badpointquant.csv", lst_gs,
    scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_negvalue.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_doublepoint.csv", lst_gs, scen_info = scen_info))
  testthat::expect_warning(validate_submission(
    "training_data/2022-01-09_highvalue.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_lowcumcase.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_lowcumdeath.csv", lst_gs, scen_info = scen_info))
  testthat::expect_warning(validate_submission(
    "training_data/2022-01-09_uniquevalue.csv", lst_gs, scen_info = scen_info))

  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_badnametarget.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_misswk.csv", lst_gs, scen_info = scen_info))
  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_badstartdate.csv", lst_gs, scen_info = scen_info))

  testthat::expect_error(validate_submission(
    "training_data/2022-01-09_badlocation.csv", lst_gs, scen_info = scen_info))
  testthat::expect_warning(validate_submission(
    "training_data/2021-11-14_no0location.csv", lst_gs, scen_info = scen_info))

  testthat::expect_error(validate_submission(
    "training_data/2noround.csv", lst_gs, scen_info = scen_info))



})
