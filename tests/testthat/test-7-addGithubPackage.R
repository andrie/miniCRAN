
test_that("can add package from github",{

  skip_on_cran()
  skip_if_offline()

  pdb <- pkgAvail(repos = c(CRAN = getOption("minicran.mran")))
  expect_type(pdb, "character")

  # Overwrite pdb with development version of miniCRAN at github
  expect_warning(
    newpdb <- addPackageListingGithub(pdb = pdb, "andrie/miniCRAN"),
    "Overwriting package information for: miniCRAN"
  )
  expect_type(newpdb, "character")
  expect_equal(nrow(pdb), nrow(newpdb))
  expect_equal(ncol(pdb), ncol(newpdb))
})
