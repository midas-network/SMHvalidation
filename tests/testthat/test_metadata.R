test_that("Test validation metadata process", {

  hub_path <- "../exp/"
  check <- SMHvalidation::validate_part_file(hub_path, "t3-mc",
                                             c("origin_date", "target"))
  test <- unique(purrr::map_vec(purrr::map(check, ~attr(.x, "class")), 1))
  expect_equal(test, c("check_success"))

  check <- SMHvalidation::validate_part_file(hub_path, "team2-modelb", NULL)
  test <- unique(purrr::map_vec(purrr::map(check, ~attr(.x, "class")), 1))
  expect_equal(test, c("check_success"))

  check <- SMHvalidation::validate_part_file(hub_path, "team2-modelb",
                                             "origin_date")
  expect_contains(attr(check$partition_structure, "class"), "check_error")

  check <- SMHvalidation::validate_part_file(hub_path, "t2-modelb",
                                             "origin_date")
  expect_contains(attr(check$file_exists, "class"), "check_error")

  check <- SMHvalidation::validate_part_file(hub_path, "team2-modelb",
                                             "origin_data")
  expect_contains(attr(check$partition_name, "class"), "check_error")

  mo_path <- "../exp/model-output/t3-mc/2023-11-12/cum%20hosp"
  file.rename(paste0(mo_path, "/2023-11-12-t3-mc0.parquet"),
              paste0(mo_path, "/2024-11-12-t3-mc0.parquet"))
  check <- SMHvalidation::validate_part_file(hub_path, "t3-mc",
                                             c("origin_date", "target"))
  expect_contains(attr(check$file_n, "class"), "check_error")
  file.rename(paste0(mo_path, "/2024-11-12-t3-mc0.parquet"),
              paste0(mo_path, "/2023-11-12-t3-mc0.parquet"))

  mo_path <- "../exp/model-output/team2-modelb"
  file.rename(paste0(mo_path, "/2023-11-12-team2-modelb.parquet"),
              paste0(mo_path, "/2024-11-12-team2-modelb.parquet"))
  check <- SMHvalidation::validate_part_file(hub_path, "team2-modelb", NULL)
  expect_contains(attr(check$round_id_valid, "class"), "check_error")
  file.rename(paste0(mo_path, "/2024-11-12-team2-modelb.parquet"),
              paste0(mo_path, "/2023-11-12-team2-modelb.parquet"))

  dir.create("../exp/model-output/t4-mc/2024-11-12/", recursive = TRUE)
  file.copy(dir("../exp/model-output/t3-mc/", full.names = TRUE,
                recursive = TRUE),
            "../exp/model-output/t4-mc/2024-11-12", recursive = TRUE)
  check <- SMHvalidation::validate_part_file(hub_path, "t4-mc", "origin_date")
  expect_contains(attr(check$partition_value, "class"), "check_error")
  unlink("../exp/model-output/t4-mc", recursive = TRUE)

})
