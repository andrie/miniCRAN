if (interactive()) {library(testthat); library(testthis)}
context("get CRAN description")

test_that("can read CRAN description", {

  skip_on_cran()
  mockery::stub(getCranDescription, 
                what = "tools::CRAN_package_db",
                function(...) testthis::read_testdata("/pdb.rds")
                )
  p <- getCranDescription("miniCRAN", repos = c(CRAN = getOption("minicran.mran")))
  expect_is(p, "data.frame")
  expect_equal(p$Package[1], "miniCRAN")
})
