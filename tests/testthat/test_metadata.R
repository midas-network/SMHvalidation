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
})
