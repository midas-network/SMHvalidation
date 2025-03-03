test_that("Test visualization", {

  tmp_dir <- tempdir()
  lst_gs <- readRDS("tst_dt/2022-08-22_lst_gs.rds")

  expect_no_error(generate_validation_plots(paste0(hub_model, "team2-modelb/",
                                                   "2023-11-12-team2-modelb",
                                                   ext), obs,
                                            save_path = tmp_dir))

  file.remove(dir(tmp_dir, full.names = TRUE)[grep("plots.pdf", dir(tmp_dir))])
  file.remove(dir(".", pattern = ".pdf$"))
})
