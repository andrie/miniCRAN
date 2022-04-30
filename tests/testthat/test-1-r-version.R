
# R.version
R.version_3.6.0 <- structure(list(
  platform = "x86_64-w64-mingw32", 
  arch = "x86_64",
  os = "mingw32", 
  system = "x86_64, mingw32", 
  status = "",
  major = "3", minor = "6.0", 
  year = "2019", month = "04", day = "26", 
  `svn rev` = "76424", language = "R", 
  version.string = "R version 3.6.0 (2019-04-26)",
  nickname = "Planting of a Tree"
), class = "simple.list")

# getRversion()
getRversion_3.6.0 <- structure(list(
  c(3L, 6L, 0L)), 
  class = c("R_system_version", "package_version", "numeric_version"))

expect_3.6 <- function(object) {
  expect_equal(object, "3.6")
}

test_that("twoDigitRversion() behaves", {
  expect_3.6(
    twodigitRversion(list(major = 3, minor = 6))
  )
  expect_3.6(
    twodigitRversion("3.6")
  )
  expect_3.6(
    twodigitRversion("3.6.0")
  )
  expect_3.6(
    twodigitRversion("3.6.1")
  )
  expect_3.6(
    twodigitRversion(R.version_3.6.0)
  )
  expect_3.6(
    twodigitRversion(getRversion_3.6.0)
  )
})
