test_that("pkgAvail throws warnings and errors for incorrect CRAN repos", {
  expect_warning(pkgAvail(repos = ""), "unable to access index for repository")

  is.available.packages <- function(x) {
    required_cols <- c("Package", "Version", "Depends", "Imports", "Suggests")
    all(
      is.matrix(x),
      nrow(x) > 0,
      required_cols %in% colnames(x)
    )
  }

  skip_if_offline(p3m())

  mran <- p3m("2024-01-02")

  expect_true(is.available.packages(
    pkgAvail(repos = mran)
  ))
})
