if(interactive()) library(testthat)
context("get CRAN description")

test_that("can read CRAN description", {
  
  skip_on_cran()
  p <- getCranDescription("miniCRAN")
  expect_is(p, "data.frame")
  expect_equal(p$Package[1], "miniCRAN")

})

