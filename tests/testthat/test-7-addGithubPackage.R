
test_that("can add package listing from github",{

  skip_on_cran()

  pdb <- pkgAvail(repos = c(CRAN = getOption("minicran.mran")))
  expect_type(pdb, "character")

  # Overwrite pdb with development version of miniCRAN at github
  expect_warning(
    newpdb <- addPackageListingGithub(pdb = pdb, "andrie/miniCRAN"),
    "Overwriting package information for: miniCRAN"
  )
  expect_type(newpdb, "character")
  expect_equal(nrow(pdb), nrow(newpdb))
  expect_equal(ncol(pdb), ncol(newpdb))
})

repo_path <- file.path(tempdir(), "cran")

test_that("can add github package", {
  
  skip_on_cran()
  
  # Create a miniCRAN repo
  unlink(repo_path, recursive = TRUE)
  dir.create(repo_path, showWarnings = FALSE)
  repo <- "https://cran.r-project.org"
  miniCRAN::makeRepo(pkgs = c(), path = repo_path, type = "win.binary", repos = repo)
  
  addPackageGitHub("andrie/miniCRAN", path = repo_path)

  expect_message(
    addPackageGitHub("andrie/miniCRAN", path = repo_path),
    "All packages up to date. Nothing to add."
  )

})
