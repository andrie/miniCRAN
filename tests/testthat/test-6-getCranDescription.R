
test_that("can read CRAN description", {

  skip_on_cran()
  skip_if_offline()
  skip_if(getRversion() <= "3.4")

  skip_if_not_installed("mockery")
  skip_if_not_installed("testthis")

  mockery::stub(
    getCranDescription,
    what = "tools::CRAN_package_db",
    function(...) {
      suppressMessages(
        testthis::read_testdata("/pdb.rds")
      )
    }
  )
  p <- getCranDescription("ggplot2", repos = MRAN("2024-01-02"))
  expect_s3_class(p, "data.frame")
  expect_equal(p$Package[1], "ggplot2")
})
