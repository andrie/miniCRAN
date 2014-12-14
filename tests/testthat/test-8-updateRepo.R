context("updateRepo")

checkForRepoFiles <- function(path, pkgList, prefix){
  ptn <- "tar\\.gz|zip|tgz"
  ff <- list.files(file.path(path, prefix), recursive = TRUE, pattern = ptn)
  if(length(ff) < length(pkgList)) return(FALSE)
  ret <- sapply(pkgList, function(x)any(grepl(x, ff)))
  if(all(ret > 0)) TRUE else {
    message(ret)
    FALSE
  }
}


# list.files(repo_root, recursive = TRUE)


skip_on_cran()


# make baseline repo ------------------------------------------------------

repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if(file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
dir.create(repo_root, recursive=TRUE, showWarnings = FALSE)
# 
# 
# 
revolution <- c(CRAN="http://mran.revolutionanalytics.com/snapshot/2014-10-15")
pkgs <- c("chron", "acss")

pdb_source <- pkgAvail(repos=revolution, type="source")
pdb_win    <- pkgAvail(repos=revolution, type="win.binary")
pdb_mac    <- pkgAvail(repos=revolution, type="mac.binary.mavericks")


pkgList_source <- pkgDep(pkgs, availPkgs=pdb_source, repos=revolution, type="source", suggests=FALSE)
# makeRepo(pkgList, path=repo_root, repos=revolution, type="source", quiet=TRUE)

pkgList_win <- pkgDep(pkgs, availPkgs=pdb_win, repos=revolution, type="win.binary", suggests=FALSE)
# makeRepo(pkgList, path=repo_root, repos=revolution, type="win.binary", quiet=TRUE)

pkgList_mac <- pkgDep(pkgs, availPkgs=pdb_mac, repos=revolution, type="mac.binary.mavericks", suggests=FALSE)
# makeRepo(pkgList, path=repo_root, repos=revolution, type="mac.binary.mavericks", quiet=TRUE)

file.copy(
  from = list.dirs(system.file("inst/sample-repo", package="miniCRAN"), recursive = FALSE),
  to = repo_root,
  recursive = TRUE
)

pkgsAdd <- c("foreach")
revolution <- c(CRAN="http://mran.revolutionanalytics.com/snapshot/2014-12-01")



# Update repo with source and binary packages -----------------------------

test_that("addPackage downloads source files and rebuilds PACKAGES file", {
  
  
  pkg_type <- "source"
  pkgList  <- pkgList_source
  
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb_source, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  
  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  
  expect_true(
    checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  
})



test_that("addPackage downloads source files and rebuilds PACKAGES file", {
  
  
  pkg_type <- "win.binary"
  pkgList  <- pkgList_win
  
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb_win, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  
  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  
  expect_true(
    checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  
})



test_that("addPackage downloads mac binary files and rebuilds PACKAGES file", {
  
  
  pkg_type <- "mac.binary"
  pkgList  <- pkgList_mac
  
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb_mac, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  
  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  
  expect_true(
    checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  
})


