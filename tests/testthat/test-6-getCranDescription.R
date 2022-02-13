
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
  p <- getCranDescription("ggplot2", repos = c(CRAN = getOption("minicran.mran")))
  expect_s3_class(p, "data.frame")
  expect_equal(p$Package[1], "ggplot2")
})

# test_that("throws error on old versions of R", {
#   skip_if_not_installed("mockery")
#   if (getRversion() > "3.4.0") {
#     # mock the version of R to force the error
#     mockery::stub(
#       getCranDescription, 
#       what = "getRversion",
#       how = "3.4.0"
#     )
#   }
#   expect_error(
#     getCranDescription("miniCRAN", repos = c(CRAN = getOption("minicran.mran"))),
#     "This function is not available"
#   )
# })
#   
# 
