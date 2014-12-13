context("addPackages")

checkForRepoFiles <- function(path, pkgList, prefix){
  f <- list.files(path, recursive=TRUE)
  ret <- sapply(pkgList, function(x) grep(sprintf("%s/%s_.*\\.(tar\\.gz|zip|tgz)", prefix, x), f))
  if(all(ret > 0)) TRUE else {
    message(ret)
    FALSE
  }
}

revolution <- c(CRAN="http://cran.revolutionanalytics.com")
pkgs <- c("foreach")
pkgsAdd <- c("snowfall")
repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if(file.exists(repo_root)) unlink(repo_root, recursive = TRUE)

# list.files(repo_root, recursive = TRUE)


# source ------------------------------------------------------------------

test_that("addPackage downloads source files and rebuilds PACKAGES file", {

  skip_on_cran()

  pkg_type <- "source"
  pdb <- pkgAvail(repos=revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  dir.create(repo_root, recursive=TRUE, showWarnings = FALSE)

  makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)

  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)

  expect_true(
    checkForRepoFiles(repo_root, pkgList, prefix)
  )
  expect_true(
    checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )

  unlink(repo_root, recursive=TRUE)

})

# windows binaries --------------------------------------------------------

test_that("addPackage downloads windows binary files and rebuilds PACKAGES file", {

  pkg_type <- "win.binary"
  pdb <- pkgAvail(repos=revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  dir.create(repo_root, recursive=TRUE, showWarnings = FALSE)

  makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)

  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)

  expect_true(
    checkForRepoFiles(repo_root, pkgList, prefix)
  )
  expect_true(
    checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )

  unlink(repo_root, recursive = TRUE)

})

# mac binaries ------------------------------------------------------------

test_that("addPackage downloads mac binary files and rebuilds PACKAGES file", {

  skip_on_cran()

  pkg_type <- "mac.binary"
  pdb <- pkgAvail(repos=revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  dir.create(repo_root, recursive=TRUE, showWarnings = FALSE)

  makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)

  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)

  expect_true(
    checkForRepoFiles(repo_root, pkgList, prefix)
  )
  expect_true(
    checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )

  unlink(repo_root, recursive = TRUE)

})

# mac mavericks binaries --------------------------------------------------

# test_that("addPackage downloads mac mavericks binary files and rebuilds PACKAGES file", {
#
#   pkg_type <- "mac.binary.mavericks"
#   pdb <- pkgAvail(repos=revolution, type=pkg_type)
#   pkgList <- pkgDep(pkgs, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
#   pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
#   prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
#   dir.create(repo_root, recursive=TRUE, showWarnings = FALSE)
#
#   makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type)
#
#   addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type)
#
#   expect_true(
#     checkForRepoFiles(repo_root, pkgList, prefix)
#   )
#   expect_true(
#     checkForRepoFiles(repo_root, pkgListAdd, prefix)
#   )
#   expect_true(
#     file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
#   )
#
#   unlink(repo_root, recursive = TRUE)
#
# })
