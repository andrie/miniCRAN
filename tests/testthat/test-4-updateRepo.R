if (interactive()) {library(testthat); Sys.setenv(NOT_CRAN = "true")}

context("updateRepo")

# make baseline repo ------------------------------------------------------


repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

revolution <- MRAN("2014-10-15")
if(!miniCRAN:::is.online(revolution, tryHttp = FALSE)) {
  # Use http:// for older versions of R
  revolution <- sub("^https://", "http://", revolution)
}
rvers <- "3.1"
pkgs <- c("chron", "adaptivetau")

types <- c("source", "win.binary", "mac.binary")
names(types) <- types

test_that("sample repo is setup correctly", {
  skip_if_offline(revolution)

  pdb <<- lapply(types, pkgAvail, repos = revolution, Rversion = rvers)
  pkgList <<- lapply(types, function(type) {
    pkgDep(pkg = pkgs, type = types[type], availPkgs = pdb[[type]],
           repos = revolution, suggests = FALSE, Rversion = rvers)
  })

  miniCRAN:::.createSampleRepo(path = repo_root, MRAN = revolution, Rversion = rvers)
  expect_equal(unname(pkgAvail(repo_root)[, "Package"]), sort(pkgs))
})


# Add packages to repo ----------------------------------------------------

pkgsAdd <- c("aprof")

for (pkg_type in names(types)) {

  context(sprintf(" - Add packages to repo (%s)", pkg_type))

  test_that(sprintf("addPackage downloads %s files and rebuilds PACKAGES file", pkg_type), {

    skip_on_cran()
    skip_if_offline(revolution)

    pkgListAdd <- pkgDep(pkgsAdd, availPkgs = pdb[[pkg_type]],
                         repos = revolution,
                         type  = pkg_type,
                         suggests = FALSE,
                         Rversion = rvers)
    prefix <- miniCRAN:::repoPrefix(pkg_type, Rversion = rvers)

    addPackage(pkgListAdd, path = repo_root, repos = revolution, type = pkg_type,
               quiet = TRUE, Rversion = rvers)

    expect_true(
      miniCRAN:::.checkForRepoFiles(repo_root, pkgListAdd, prefix)
    )
    expect_true(
      file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
    )
    expect_true(
      all(
        pkgListAdd %in% pkgAvail(repo_root, type = pkg_type, Rversion = rvers)[, "Package"]
      )
    )
  })
}



# Check for updates -------------------------------------------------------


MRAN_mirror <- MRAN("2014-12-01")
if(!miniCRAN:::is.online(MRAN_mirror, tryHttp = FALSE)) {
  # Use http:// for older versions of R
  MRAN_mirror <- sub("^https://", "http://", revolution)
}

for (pkg_type in names(types)) {
  context(sprintf(" - Check for updates (%s)", pkg_type))

  test_that(sprintf("updatePackages downloads %s files and builds PACKAGES file", pkg_type), {

    skip_on_cran()
    skip_if_offline(MRAN_mirror)

    prefix <- miniCRAN:::repoPrefix(pkg_type, Rversion = rvers)

    old <- oldPackages(path = repo_root, repos = MRAN_mirror, type = pkg_type, Rversion = rvers)

    expect_equal(nrow(old), 2)
    expect_equal(ncol(old), 4)
    expect_equal(rownames(old), c("adaptivetau", "aprof"))

    updatePackages(path = repo_root, repos = MRAN_mirror, type = pkg_type,
                   ask = FALSE, quiet = TRUE, Rversion = rvers)

    updateVers <- miniCRAN:::getPkgVersFromFile(list.files(file.path(repo_root, prefix)))

    expect_true(
      miniCRAN:::.checkForRepoFiles(repo_root, pkgList[[pkg_type]], prefix)
    )

    expect_true(
      file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
    )

    old <- oldPackages(path = repo_root, repos = MRAN_mirror, type = pkg_type, Rversion = rvers)
    # browser()
    expect_equal(nrow(old), 0)
    expect_equal(ncol(old), 4)

  })
}


# Check for duplicate packages --------------------------------------------

context("Check for duplicate files")

for (pkg_type in names(types)) {

  test_that(sprintf("checkVersions() finds out-of-date %s packages", pkg_type), {

    skip_on_cran()
    skip_if_offline(MRAN_mirror)

    oldVersions <- list(package = c("aprof"),
                        version = c("0.2.1"))
    if (pkg_type != "source") {
      expect_error(
        addOldPackage(oldVersions[["package"]], path = repo_root, vers = oldVersions[["version"]],
                      repos = MRAN_mirror, type = pkg_type)
      )
    } else {
      addOldPackage(oldVersions[["package"]], path = repo_root, vers = oldVersions[["version"]],
                    repos = MRAN_mirror, type = pkg_type)
      files <- suppressWarnings(
        checkVersions(path = repo_root, type = pkg_type)
      )

      expect_true(
        all(file.exists(files))
      )

      pkgs <- sapply(strsplit(basename(files), "_"), "[[", 1)
      dupes <- pkgs[duplicated(pkgs)]
      expect_true(
        all(dupes == oldVersions[["package"]])
      )

    }
  })
}
