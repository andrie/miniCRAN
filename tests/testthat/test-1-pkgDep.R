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
  expect_is(exp, "pkgDep")
  expect_identical(
    as.vector(exp), 
    c("plyr", "digest", "gtable", "reshape2", "scales", 
      "proto", "MASS", "Rcpp", "stringr", "RColorBrewer", "dichromat", 
      "munsell", "labeling", "colorspace", "ggplot2")
  )
  
  exp <- pkgDep("ggplot2", availPkgs = cranJuly2014, suggests=TRUE)
  expect_is(exp, "pkgDep")
  expect_identical(
    as.vector(exp), 
    c("plyr", "digest", "gtable", "reshape2", "scales", 
      "proto", "MASS", "Rcpp", "stringr", "RColorBrewer", "dichromat", 
      "munsell", "labeling", "colorspace", "SparseM", "lattice", "survival", 
      "Formula", "latticeExtra", "cluster", "maps", "sp", "foreign", 
      "mvtnorm", "TH.data", "sandwich", "zoo", "evaluate", "formatR", 
      "highr", "markdown", "mime", "nlme", "Matrix", "ggplot2", "quantreg", 
      "Hmisc", "mapproj", "hexbin", "maptools", "multcomp", "testthat", 
      "knitr", "mgcv")
  )
  
})


# includeBasePkgs ---------------------------------------------------------

test_that("pkgDep treats includeBasePkgs correctly", {
  exp <- pkgDep("reshape2", includeBasePkgs=TRUE, availPkgs = cranJuly2014)
  expect_is(exp, "pkgDep")
  expect_identical(
    as.vector(exp), 
    c("plyr", "stringr", "Rcpp", "methods", "reshape2")
  )
  
  exp <- pkgDep("reshape2", includeBasePkgs=FALSE, availPkgs = cranJuly2014)
  expect_is(exp, "pkgDep")
  expect_identical(
    as.vector(exp), 
    c("plyr", "stringr", "Rcpp", "reshape2")
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