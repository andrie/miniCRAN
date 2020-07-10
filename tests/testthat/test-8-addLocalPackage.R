context("addLocalPackage")

# Create a helper function to make a fake package in tempdir()

make_fake_package <- function(version = "0.1.0", base_path = tempdir()) {
  fake_package <- file.path(base_path, "fake.package")
  dir.create(fake_package, showWarnings = FALSE)
  
  # Create a fake function to add to the package
  foo <- function(x)NA
  
  # Create the skeleton
  
  if (getRversion() >= "3.5") {
    package.skeleton(
      "fake.package", 
      path = base_path, 
      list = "foo",
      force = TRUE, 
      environment = environment(foo),
      encoding = "UTF-8"
    )
  } else {
    package.skeleton(
      "fake.package", 
      path = base_path, 
      list = "foo",
      force = TRUE, 
      environment = environment(foo)
    )
  }
  
  # Remove unnecessary detritus from skeleton
  file.remove(file.path(fake_package, "NAMESPACE"))
  unlink(file.path(fake_package, "data"), recursive = TRUE)
  unlink(file.path(fake_package, "man"), recursive = TRUE)
  unlink(file.path(fake_package, "Read-and-delete-me"), recursive = TRUE)

    # Write a function file with some roxygen
  writeLines(
    con = file.path(fake_package, "R", "foo.R"),
    text = "
  #' Foo.
  #' 
  #' Does nothing.
  #' @export
  #' foo <- function(x)NULL
  
  ")
  
  # Set package version
  desc <- readLines(file.path(fake_package, "DESCRIPTION"))
  version_line <- grep("^Version:", desc)
  desc[version_line] <- paste0("Version: ", version)
  writeLines(desc, con = file.path(file.path(fake_package, "DESCRIPTION")))
 
  # Document the package
  capture.output(
    devtools::document(fake_package)
  )
  
  # Build the package
  devtools::build(fake_package, path = base_path, quiet = TRUE)
}


# Create fake.package

list_fake_pkg <- function(path = tempdir()) {
  list.files(path = path, pattern = "fake.package_.*.tar.gz", full.names = TRUE)
}


# tests start -------------------------------------------------------------
repo_path <- file.path(tempdir(), "cran")

test_that("can add fake local package", {
  
  skip_on_cran()
  
  # create the fake package
  fake_path <- file.path(tempdir(), "fake.package")
  unlink(fake_path, recursive = TRUE)
  unlink(list_fake_pkg())
  
  make_fake_package("0.1.0")
  
  expect_true(length(list_fake_pkg()) == 1)
  
  # Create a miniCRAN repo
  unlink(repo_path, recursive = TRUE)
  dir.create(repo_path, showWarnings = FALSE)
  repo <- "https://cran.r-project.org"
  miniCRAN::makeRepo(pkgs = c(), path = repo_path, type = "win.binary", repos = repo)
  
  expect_message(
    addLocalPackage(pkgs = "fake.package", pkgPath = tempdir(), path = repo_path),
    "copying fake.package_0.1.0.tar.gz"
  )
  expect_message(
    addLocalPackage(pkgs = "fake.package", pkgPath = tempdir(), path = repo_path),
    "All packages up to date. Nothing to add."
  )
  
})


test_that("can update fake local package", {
  
  skip_on_cran()
  
  
  # Update fake.package
  
  make_fake_package("0.2.0")
  expect_message(
    addLocalPackage(pkgs = "fake.package", pkgPath = tempdir(), path = repo_path),
    "copying fake.package_0.2.0.tar.gz"
  )
})
