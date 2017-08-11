if (interactive()) library(testthat)
context("pkgDep")

test_that("can add package from github",{

  skip_on_cran()

  pdb <- pkgAvail(repos = c(CRAN = getOption("minicran.mran")))
  expect_is(pdb, "matrix")

  # Overwrite pdb with development version of miniCRAN at github
  expect_warning(
    newpdb <- addPackageListingGithub(pdb = pdb, "andrie/miniCRAN"),
    "Overwriting package information for: miniCRAN"
  )
  expect_is(newpdb, "matrix")
  expect_equal(nrow(pdb), nrow(newpdb))
  expect_equal(ncol(pdb), ncol(newpdb))
})
