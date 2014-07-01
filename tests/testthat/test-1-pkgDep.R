context("Test pkgDep")



# incorrect package list --------------------------------------------------

test_that("pkgDep throws warnings and errors", {
  
  expect_error(
    pkgDep(, availPkgs = cranJuly2014), 
    "pkg should be a character vector with package names"
  )
  
  expect_error(
    pkgDep(matrix(1:3, 3), availPkgs = cranJuly2014), 
    "pkg should be a character vector with package names"
  )
  
  expect_error(
    pkgDep("reshape99", availPkgs = cranJuly2014), 
    "No valid packages in pkg"
  )
  
  expect_error(
    pkgDep(c("reshape98", "reshape99"), availPkgs = cranJuly2014), 
    "No valid packages in pkg"
  )
  
  expect_warning(
    pkgDep(c("reshape2", "reshape99"), availPkgs = cranJuly2014), 
    "Package not recognized: reshape99"
  )
  
  
})


# suggests ----------------------------------------------------------------

test_that("pkgDep treats suggests correctly", {
  
  exp <- pkgDep("ggplot2", availPkgs = cranJuly2014, suggests=FALSE)
  expect_identical(
    exp, 
    c("colorspace", "dichromat", "digest", "ggplot2", "gtable", "labeling", 
      "MASS", "munsell", "plyr", "proto", "RColorBrewer", "Rcpp", "reshape2", 
      "scales", "stringr")
  )
  
  exp <- pkgDep("ggplot2", availPkgs = cranJuly2014, suggests=TRUE)
  expect_identical(
    exp, 
    c("colorspace", "dichromat", "digest", "ggplot2", "gtable", "hexbin", 
      "Hmisc", "knitr", "labeling", "mapproj", "maps", "maptools", 
      "MASS", "mgcv", "multcomp", "munsell", "nlme", "plyr", "proto", 
      "quantreg", "RColorBrewer", "Rcpp", "reshape2", "scales", "stringr", 
      "testthat")
  )
  
})


# includeBasePkgs ---------------------------------------------------------

test_that("pkgDep treats includeBasePkgs correctly", {
  exp <- pkgDep("reshape2", includeBasePkgs=TRUE, availPkgs = cranJuly2014)
  expect_identical(
    exp, 
    c("methods", "plyr", "Rcpp", "reshape2", "stringr")
  )
  
  exp <- pkgDep("reshape2", includeBasePkgs=FALSE, availPkgs = cranJuly2014)
  expect_identical(
    exp, 
    c("plyr", "Rcpp", "reshape2", "stringr")
  )
  
})



# CRAN mirror -------------------------------------------------------------

test_that("pkgDep throws warnings and errors for incorrect CRAN repos", {
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

  
})