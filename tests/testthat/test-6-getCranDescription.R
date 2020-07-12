if (interactive()) {library(testthat); library(testthis)}

test_that("can read CRAN description", {

  skip_on_cran()
  skip_if(getRversion() <= "3.4")

  skip_if_not_installed("mockery")
  mockery::stub(
    getCranDescription,
    what = "tools::CRAN_package_db",
    function(...) testthis::read_testdata("/pdb.rds")
  )
  p <- getCranDescription("miniCRAN", repos = c(CRAN = getOption("minicran.mran")))
  expect_is(p, "data.frame")
  expect_equal(p$Package[1], "miniCRAN")
})

test_that("throws error on old versions of R", {
  if (getRversion() > "3.4.0") {
    # mock the version of R to force the error
    mockery::stub(
      getCranDescription, 
      what = "getRversion",
      function() "3.4.0",
    )
  }
  expect_error(
    getCranDescription("miniCRAN", repos = c(CRAN = getOption("minicran.mran"))),
    "This function is not available"
  )
})
  

