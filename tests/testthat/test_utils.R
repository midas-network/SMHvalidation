test_that("Test utility functions", {

  expect_false(SMHvalidation:::is_wholenumber(NA))

  expect_no_error(read_files("./tst_dt/2021-08-15_morewk.gz"))
  expect_no_error(read_files("./tst_dt/2021-11-14_no0location.zip"))

  path <- "./tst_dt/partition_err/"
  expect_no_error(SMHvalidation:::load_partition_arrow(path,
                                                       partition = "target"))

  expect_error(SMHvalidation:::load_partition_arrow("./tst_dt/partition_err2/",
                                                    partition = "target"))

})
