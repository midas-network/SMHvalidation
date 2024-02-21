test_that("Test visualization", {

  tmp_dir <- tempdir()
  lst_gs <- readRDS("tst_dt/2022-08-22_lst_gs.rds")

  expect_no_error(generate_validation_plots("tst_dt/2022-01-09_no_error.csv",
                                            lst_gs, save_path = tmp_dir))

  expect_no_error(generate_validation_plots("tst_dt/2022-01-09_highvalue.csv",
                                            NULL, save_path = tmp_dir,
                                            y_sqrt = TRUE,
                                            plot_quantiles = c(0.25, 0.75)))

  file.remove(dir(tmp_dir, full.names = TRUE)[grep("plots.pdf", dir(tmp_dir))])
  file.remove(dir(".", pattern = ".pdf$"))
})
