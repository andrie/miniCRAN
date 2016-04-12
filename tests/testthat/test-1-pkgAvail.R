context("Test pkgAvail")

test_that("pkgAvail throws warnings and errors for incorrect CRAN repos", {
  expect_warning(pkgAvail(repos=""))
  
  is.available.packages <- function(x){
    all(is.matrix(x), dim(x)[2] == 17, names(x)[1:3] == c("Package", "Version","Priority", "Depends"))
  }
  
  expect_true(is.available.packages(
    pkgAvail(repos="http://cran.revolutionanalytics.com")
  ))
  expect_true(is.available.packages(
    pkgAvail(repos=c(CRAN="http://cran.revolutionanalytics.com"))
  ))
  expect_true(is.available.packages(
    pkgAvail(repos=c(CRAN="@CRAN@"))
  ))
  
  
  expect_true(is.available.packages(
    pkgAvail(c(REVO = "https://mran.microsoft.com"))
  ))
})