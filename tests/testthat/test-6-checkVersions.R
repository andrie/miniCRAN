context("checkVersions")

checkForRepoFiles <- function(path, pkgList, prefix){
  f <- list.files(path, recursive = TRUE)
  ret <- sapply(pkgList, function(x) grep(sprintf("%s/%s_.*\\.(tar\\.gz|zip|tgz)", prefix, x), f))
  if(all(ret > 0)) TRUE else {
    message(ret)
    FALSE
  }
}

revolution <- c(CRAN="http://mran.revolutionanalytics.com/snapshot/2014-12-01")
pkgs <- c("foreach")
repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if(file.exists(repo_root)) unlink(repo_root, recursive = TRUE)

# list.files(repo_root, recursive = TRUE)

# source ------------------------------------------------------------------

test_that("checkVersions downloads old and current source files checks for these duplicate versions", {

  skip_on_cran()

  pkg_type <- "source"
  pdb <- pkgAvail(repos = revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

  # pkgList should yield c("foreach", "codetools", "iterators")
  # which should correspond to c("1.4.2", "0.2-9", "1.0.7") or higher
  oldVers <- data.frame(package=c("foreach", "codetools", "iterators"),
                        version=c("1.4.0", "0.2-7", "1.0.5"),
                        stringsAsFactors=FALSE)
  addOldPackage(pkgList, path=repo_root, vers=oldVers[,"version"],
                repos=revolution, type=pkg_type)
  makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type, writePACKAGES=FALSE, quiet=TRUE)

  files <- suppressWarnings(checkVersions(pkgList, path=repo_root, type=pkg_type))

  expect_true(
    all(file.exists(files))
  )

  pkgs <- sapply(strsplit(basename(files), "_"), "[[", 1)
  dupes <- pkgs[duplicated(pkgs)]
  expect_true(
    all(dupes==pkgList)
  )

  unlink(repo_root, recursive = TRUE)

})
