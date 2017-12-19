if (require(testthat)) {
  source(file.path("testthat", "helpers.R"))
  test_check("miniCRAN", filter = "pkgDep")
}
