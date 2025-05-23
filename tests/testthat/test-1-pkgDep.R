if (interactive()) library(testthat)

# incorrect package list --------------------------------------------------

test_that("pkgDep throws warnings and errors", {
  expect_error(
    pkgDep(availPkgs = cranJuly2014),
    "argument \"pkg\" is missing, with no default"
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
  exp <- pkgDep("ggplot2", availPkgs = cranJuly2014, suggests = FALSE)
  expect_s3_class(exp, "pkgDep")
  expect_identical(
    as.vector(exp),
    c(
      "ggplot2",
      "plyr",
      "digest",
      "gtable",
      "reshape2",
      "scales",
      "proto",
      "MASS",
      "Rcpp",
      "stringr",
      "RColorBrewer",
      "dichromat",
      "munsell",
      "labeling",
      "colorspace"
    )
  )

  exp <- pkgDep("ggplot2", availPkgs = cranJuly2014, suggests = TRUE)
  expect_s3_class(exp, "pkgDep")
  expect_identical(
    as.vector(exp),
    c(
      "ggplot2",
      "plyr",
      "digest",
      "gtable",
      "reshape2",
      "scales",
      "proto",
      "MASS",
      "Rcpp",
      "stringr",
      "RColorBrewer",
      "dichromat",
      "munsell",
      "labeling",
      "colorspace",
      "SparseM",
      "lattice",
      "survival",
      "Formula",
      "latticeExtra",
      "cluster",
      "maps",
      "sp",
      "foreign",
      "mvtnorm",
      "TH.data",
      "sandwich",
      "zoo",
      "evaluate",
      "formatR",
      "highr",
      "markdown",
      "mime",
      "nlme",
      "Matrix",
      "quantreg",
      "Hmisc",
      "mapproj",
      "hexbin",
      "maptools",
      "multcomp",
      "testthat",
      "knitr",
      "mgcv"
    )
  )
})


# includeBasePkgs ---------------------------------------------------------

test_that("pkgDep treats includeBasePkgs correctly", {
  exp <- pkgDep(
    "reshape2",
    includeBasePkgs = TRUE,
    availPkgs = cranJuly2014,
    suggests = FALSE
  )
  expect_s3_class(exp, "pkgDep")
  expect_identical(
    as.vector(exp),
    c("reshape2", "plyr", "stringr", "Rcpp", "methods")
  )

  exp <- pkgDep(
    "reshape2",
    includeBasePkgs = FALSE,
    availPkgs = cranJuly2014,
    suggests = FALSE
  )
  expect_s3_class(exp, "pkgDep")
  expect_identical(
    as.vector(exp),
    c("reshape2", "plyr", "stringr", "Rcpp")
  )
})
