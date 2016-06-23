if (interactive()) {library(testthat); Sys.setenv(NOT_CRAN = "true")}

context("makeRepo")

revolution <- MRAN("2014-10-15")
rvers = "3.2"
pkgs <- c("Bmix")
repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)

# list.files(repo_root, recursive = TRUE)


types <- c("source", "win.binary", "mac.binary", "mac.binary.mavericks")
names(types) <- c("source", "win.binary", "mac.binary", "mac.binary")

for (pkg_type in names(types)) {
  test_that(sprintf("makeRepo downloads %s files and builds PACKAGES file", pkg_type), {
    skip_on_cran()
    skip_if_offline()

    pdb <- pkgAvail(repos = revolution, type = pkg_type, Rversion = rvers)
    pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = revolution, type = pkg_type,
                      suggests = FALSE, Rversion = rvers)
    prefix <- miniCRAN:::repoPrefix(pkg_type, Rversion = rvers)
    dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

    makeRepo(pkgList, path = repo_root, repos = revolution, type = pkg_type, quiet = TRUE, Rversion = rvers)

    expect_true(
      miniCRAN:::.checkForRepoFiles(repo_root, pkgList, prefix)
    )
    expect_true(
      file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
    )
    expect_true(
      all(
        pkgList %in% pkgAvail(repos = repo_root, type = pkg_type, Rversion = rvers)[, "Package"]
      )
    )
  })
}

unlink(repo_root, recursive = TRUE)
