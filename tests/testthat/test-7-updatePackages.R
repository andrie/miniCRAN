context("updatePackages")

checkForRepoFiles <- function(path, pkgList, prefix) {
  f <- list.files(path, recursive=TRUE)
  ret <- sapply(pkgList, function(x) grep(sprintf("%s/%s_.*\\.(tar\\.gz|zip|tgz)", prefix, x), f))
  if(all(ret > 0)) TRUE else {
    message(ret)
    FALSE
  }
}

revolution <- c(CRAN="http://cran.revolutionanalytics.com")
pkgs <- c("foreach")
repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if(file.exists(repo_root)) unlink(repo_root, recursive = TRUE)

# list.files(repo_root, recursive = TRUE)


# source ------------------------------------------------------------------

test_that("updatePackages downloads source files and builds PACKAGES file", {

  skip_on_cran()

  pkg_type <- "source"
  pdb <- pkgAvail(repos=revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs=pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  dir.create(repo_root, recursive=TRUE)

  # pkgList should yield c("foreach", "codetools", "iterators")
  # which should correspond to c("1.4.2", "0.2-9", "1.0.7") or higher
  oldVers <- data.frame(package=c("foreach", "codetools", "iterators"),
                        version=c("1.4.0", "0.2-7", "1.0.5"),
                        stringsAsFactors=FALSE)
  addOldPackage(pkgList, path=repo_root, vers=oldVers[,"version"], type=pkg_type)
  updatePackages(path=repo_root, repos=revolution, type=pkg_type, ask=FALSE)

  updateVers <- miniCRAN:::getPkgVersFromFile(list.files(file.path(repo_root, prefix)))

  expect_true(
    checkForRepoFiles(repo_root, pkgList, prefix)
  )

  pkgs1 = oldVers[ order(oldVers$package), ]$package
  pkgs2 = updateVers[ which(updateVers$package %in% pkgList), ]$package
  expect_true(
    all(pkgs1==pkgs2)
  )
  vers1 = oldVers[ order(oldVers$package), ]$version
  vers2 = updateVers[ which(updateVers$package %in% pkgList), ]$version
  expect_true(
    all(vers1!=vers2)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )

  unlink(repo_root, recursive=TRUE)

})

# windows binaries --------------------------------------------------------

test_that("updatePackages downloads windows binary files and builds PACKAGES file", {

  pkg_type <- "win.binary"
  pdb <- pkgAvail(repos = revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, miniCRAN:::twodigitRversion(R.version))
  dir.create(repo_root, recursive = TRUE)

  # pkgList should yield c("foreach", "codetools", "iterators")
  # which should correspond to c("1.4.2", "0.2-9", "1.0.7") or higher
  oldVers <- data.frame(package=c("foreach", "codetools", "iterators"),
                        version=c("1.4.0", "0.2-7", "1.0.5"),
                        stringsAsFactors=FALSE)

  expect_error(
    addOldPackage(pkgList, path=repo_root, vers=oldVers[,"version"], type=pkg_type)
  )

  unlink(repo_root, recursive=TRUE)

})

# mac binaries ------------------------------------------------------------

test_that("updatePackages downloads mac binary files and builds PACKAGES file", {

  skip_on_cran()

  pkg_type <- "mac.binary"
  pdb <- pkgAvail(repos = revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, miniCRAN:::twodigitRversion(R.version))
  dir.create(repo_root, recursive = TRUE)

  # pkgList should yield c("foreach", "codetools", "iterators")
  # which should correspond to c("1.4.2", "0.2-9", "1.0.7") or higher
  oldVers <- data.frame(package=c("foreach", "codetools", "iterators"),
                        version=c("1.4.0", "0.2-7", "1.0.5"),
                        stringsAsFactors=FALSE)

  expect_error(
    addOldPackage(pkgList, path=repo_root, vers=oldVers[,"version"], type=pkg_type)
  )

  unlink(repo_root, recursive=TRUE)

})

# mac mavericks binaries --------------------------------------------------

# test_that("updatePackages downloads mac mavericks binary files and builds PACKAGES file", {
#
#   pkg_type <- "mac.binary.mavericks"
#   pdb <- pkgAvail(repos = revolution, type=pkg_type)
#   contrib.url(revolution, pkg_type)
#   pdb <- available.packages(contrib.url(revolution, pkg_type), type=pkg_type)
#   pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
#   R <- list(major="3", minor="1")
#   prefix <- miniCRAN:::repoPrefix(pkg_type, miniCRAN:::twodigitRversion(R))
#   dir.create(repo_root, recursive = TRUE)
#   pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
#
#
#     # pkgList should yield c("foreach", "codetools", "iterators")
#     # which should correspond to c("1.4.2", "0.2-9", "1.0.7") or higher
#     oldVers <- data.frame(package=c("foreach", "codetools", "iterators"),
#                           version=c("1.4.0", "0.2-7", "1.0.5"),
#                           stringsAsFactors=FALSE)
#
#     expect_error(
#       addOldPackage(pkgList, path=repo_root, vers=oldVers[,"version"], type=pkg_type)
#     )
#
#     unlink(repo_root, recursive=TRUE)
#
#
# })
