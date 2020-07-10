if (interactive()) library(testthat)

# Copyright (c) Andrie de Vries 2018

context("github-functions")

test_that("package file extension is computed correctly", {
  expect_equal(pkgFileExt("source"), ".tar.gz")
  expect_equal(pkgFileExt("win.binary"), ".zip")
  expect_equal(pkgFileExt("mac.binary"), ".tgz")
  expect_error(pkgFileExt("nonsense"))
})

test_that("package verson is extracted correctly", {
  skip_on_cran()
  ff <- c(
    "KernSmooth_2.23-15.tar.gz",
    "MASS_7.3-47.tar.gz",
    "Matrix_1.2-12.tar.gz",
    "boot_1.3-20.tar.gz"
  )
  
  act <- getPkgVersFromFile(ff)
  exp <- data.frame(
    package = c("boot", "KernSmooth", "MASS", "Matrix"),
    version = c("1.3-20", "2.23-15", "7.3-47", "1.2-12"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(sort(act$package), sort(exp$package))
  expect_equal(sort(act$version), sort(exp$version))
  
  
  expect_equal(
    getPkgVersFromFile("nonsense"),
    data.frame(package = character(0), version = character(0))
  )
  
})

test_that("readDescription reads file", {
  sf <- system.file("DESCRIPTION", package = "miniCRAN")
  desc <- readDescription(sf)
  
  expect_is(desc, "list")
  expect_true(
    all(c("Imports", "Suggests", "Package") %in% names(desc))
  )
})
