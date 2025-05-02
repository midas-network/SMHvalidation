test_that("Test visualization", {

  tmp_dir <- tempdir()
  obs <- read.csv("../exp/target-data/time-series.csv")
  hub_model <- "../exp/model-output/"
  ext <- ".parquet"
  path <- paste0(hub_model,
                 "team2-modelb/2023-11-12-team2-modelb.parquet")
  path_f <- paste0(hub_model,
                   "team4-modeld/2023-11-12-team4-modeld.parquet")
  df0 <- arrow::read_parquet(path)


  expect_no_error(generate_validation_plots(path, obs,
                                            save_path = tmp_dir))

  expect_no_error(generate_validation_plots(paste0(hub_model, "team2-modelb/",
                                                   "2023-11-12-team2-modelb",
                                                   ext),
                                            save_path = tmp_dir,
                                            y_sqrt = TRUE))

  expect_no_error(generate_validation_plots(paste0(hub_model, "t3-mc/"),
                                            obs, save_path = tmp_dir,
                                            partition = c("origin_date",
                                                          "target")))

  df <- dplyr::filter(df0, .data[["output_type"]] == "sample")
  arrow::write_parquet(df, path_f)
  rm(df)

  expect_message(generate_validation_plots(path_f, obs, save_path = tmp_dir))


  df <- dplyr::filter(df0, .data[["output_type"]] == "quantile" &
                        grepl("cum ", .data[["target"]], fixed = TRUE))
  arrow::write_parquet(df, path_f)
  rm(df)

  expect_no_error(generate_validation_plots(path_f,
                                            save_path = tmp_dir))

  df <- dplyr::filter(df0, .data[["output_type"]] == "quantile" &
                        grepl("inc ", .data[["target"]], fixed = TRUE))
  arrow::write_parquet(df, path_f)
  rm(df)

  expect_no_error(generate_validation_plots(path_f, obs,
                                            save_path = tmp_dir))

  df <- dplyr::mutate(df0,
                      race_ethnicity = ifelse(.data[["age_group"]] == "0-130",
                                              "overall", "others"))
  arrow::write_parquet(df, path_f)
  rm(df)
  expect_no_error(generate_validation_plots(path_f, obs,
                                            save_path = tmp_dir))

  file.remove(dir(tmp_dir, full.names = TRUE)[grep("plots.pdf", dir(tmp_dir))])
  file.remove(dir(".", pattern = ".pdf$"))
})
