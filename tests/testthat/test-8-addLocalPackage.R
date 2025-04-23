# Create a helper function to make a fake package in tempdir()

# Create fake.package

list_fake_pkg <- function(path = tempdir()) {
  list.files(path = path, pattern = "fake.package_.*.tar.gz", full.names = TRUE)
}


# tests start -------------------------------------------------------------
repo_path <- file.path(tempdir(), "cran")

test_that("can add fake local package", {
  skip_on_cran()
  skip_if_offline()

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
  miniCRAN::makeRepo(
    pkgs = c(),
    path = repo_path,
    type = "win.binary",
    repos = repo,
    quiet = TRUE
  )

  expect_message(
    addLocalPackage(
      pkgs = "fake.package",
      pkgPath = tempdir(),
      path = repo_path
    ),
    "copying fake.package_0.1.0.tar.gz"
  )
  expect_message(
    addLocalPackage(
      pkgs = "fake.package",
      pkgPath = tempdir(),
      path = repo_path
    ),
    "All packages up to date. Nothing to add."
  )
})


test_that("can update fake local package", {
  skip_on_cran()

  # Update fake.package

  make_fake_package("0.2.0")
  expect_message(
    addLocalPackage(
      pkgs = "fake.package",
      pkgPath = tempdir(),
      path = repo_path
    ),
    "copying fake.package_0.2.0.tar.gz"
  )
})
