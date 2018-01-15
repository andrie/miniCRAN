if (interactive()) library(testthat)
context("get CRAN description")

test_that("can read CRAN description", {

  skip_on_cran()
  mockery::stub(getCranDescription, 
                what = "tools::CRAN_package_db",
                function(...) readRDS("mock_data/pdb.rds")
                )
  p <- getCranDescription("miniCRAN", repos = c(CRAN = getOption("minicran.mran")))
  expect_is(p, "data.frame")
  expect_equal(p$Package[1], "miniCRAN")
})
