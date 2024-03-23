

test_that("pkgAvail throws warnings and errors for incorrect CRAN repos", {
  expect_warning(pkgAvail(repos = ""), "unable to access index for repository")

  is.available.packages <- function(x) {
    all(is.matrix(x), dim(x)[2] == 17, names(x)[1:3] == c("Package", "Version", "Priority", "Depends"))
  }

  skip_if_offline(MRAN())
  
  mran <- MRAN("2024-01-02")

  expect_true(is.available.packages(
    pkgAvail(repos = mran)
  ))
  expect_true(is.available.packages(
    pkgAvail(repos = mran)
  ))
})
