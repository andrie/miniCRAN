if (interactive()) {library(testthat); Sys.setenv(NOT_CRAN = "true")}

context("makeRepo")

{
  revolution <- MRAN("2014-10-15")
  if (!is.online(revolution, tryHttp = FALSE)) {
    # Use http:// for older versions of R
    revolution <- sub("^https://", "http://", revolution)
  }
  rvers = "3.1"
  pkgs <- c("Bmix")
  repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
  
  # list.files(repo_root, recursive = TRUE)
  
  unlink(list.files(tempdir(), pattern = ".rds$", full.names = TRUE))
  
}

types <- c("source", "win.binary", "mac.binary", "mac.binary.mavericks")

for (pkg_type in (types)) {
  context(sprintf(" - %s", pkg_type))
  test_that(sprintf("makeRepo downloads %s files and builds PACKAGES",
                    pkg_type), {

    skip_on_cran()
    skip_if_offline()
    
    pdb <- pkgAvail(repos = revolution, type = pkg_type, Rversion = rvers, quiet = TRUE)
    pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = revolution, type = pkg_type,
                      suggests = FALSE, Rversion = rvers, quiet = FALSE)
    prefix <- repoPrefix(pkg_type, Rversion = rvers)
    dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

    ret <- makeRepo(pkgList, path = repo_root, repos = revolution,
                    type = pkg_type, quiet = TRUE, Rversion = rvers)

    expect_is(ret, "character")
    expect_equal(length(ret), length(pkgList))

    expect_true(
      .checkForRepoFiles(repo_root, pkgList, prefix)
    )
    expect_true(
      file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
    )
    expect_true(
      all(
        pkgList %in% pkgAvail(repos = repo_root, 
                              type = pkg_type, 
                              Rversion = rvers)[, "Package"]
      )
    )
  })
}

unlink(repo_root, recursive = TRUE)
