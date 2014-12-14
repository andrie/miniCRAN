context("Add packages to repo")

# make baseline repo ------------------------------------------------------

repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if(file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
dir.create(repo_root, recursive=TRUE, showWarnings = FALSE)

revolution <- c(CRAN="http://mran.revolutionanalytics.com/snapshot/2014-10-15")
pkgs <- c("chron", "acss")

types <- c(source="source", win="win.binary", mac="mac.binary.mavericks")

pdb <- lapply(types, pkgAvail, repos=revolution)
pkgList <- lapply(names(types), function(type){
  pkgDep(pkg=pkgs, type=types[type], availPkgs=pdb[[type]], repos=revolution, suggests=FALSE)
})

miniCRAN:::.copySampleRepo(path=repo_root)

pkgsAdd <- c("foreach")


# Add packages to repo ----------------------------------------------------


test_that("addPackage downloads source files and rebuilds PACKAGES file", {
  
  skip_on_cran()
  
  pkg_type <- "source"
  pkgList  <- pkgList[["source"]]
  
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb[["source"]], repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  
  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  
  expect_true(
    miniCRAN:::.checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  expect_true(
    all(
      pkgListAdd %in% pkgAvail(repo_root, type=pkg_type)[, "Package"]
    )
  )
  
})



test_that("addPackage downloads source files and rebuilds PACKAGES file", {

  skip_on_cran()
  
  pkg_type <- "win.binary"
  pkgList  <- pkgList[["win"]]
  
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb[["win"]], repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  
  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  
  expect_true(
    miniCRAN:::.checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  expect_true(
    all(
      pkgListAdd %in% pkgAvail(repo_root, type=pkg_type)[, "Package"]
    )
  )
  
})



test_that("addPackage downloads mac binary files and rebuilds PACKAGES file", {
  
  skip_on_cran()
  
  pkg_type <- "mac.binary"
  
  pkgListAdd <- pkgDep(pkgsAdd, availPkgs=pdb[["mac"]], repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  
  addPackage(pkgListAdd, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  
  expect_true(
    miniCRAN:::.checkForRepoFiles(repo_root, pkgListAdd, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  expect_true(
    all(
      pkgListAdd %in% pkgAvail(repo_root, type=pkg_type)[, "Package"]
    )
  )
  
})




# Check for updates -------------------------------------------------------



context("Check for updates")

revolution <- c(CRAN="http://mran.revolutionanalytics.com/snapshot/2014-12-01")


test_that("updatePackages downloads source files and builds PACKAGES file", {
  
  skip_on_cran()
  
  pkg_type <- "mac.binary.mavericks"

  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  
  old <- oldPackages(path=repo_root, repos=revolution, type=pkg_type)
  expect_equal(nrow(old), 1)
  expect_equal(ncol(old), 4)
  
  updatePackages(path=repo_root, repos=revolution, type=pkg_type, ask=FALSE, quiet=TRUE)
  
  updateVers <- miniCRAN:::getPkgVersFromFile(list.files(file.path(repo_root, prefix)))
  
  expect_true(
    miniCRAN:::.checkForRepoFiles(repo_root, pkgList[[pkg_type]], prefix)
  )
  
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  
  old <- oldPackages(path=repo_root, repos=revolution, type=pkg_type)
  expect_equal(nrow(old), 0)
  expect_equal(ncol(old), 4)
  
  
})


context("Check for duplicate files")


# Check for duplicate packages --------------------------------------------

test_that("checkVersions downloads old and current source files checks for these duplicate versions", {
  
  skip_on_cran()
  
  pkg_type <- "mac.binary.mavericks"
  
  files <- suppressWarnings(checkVersions(path=repo_root, type=pkg_type))
  
  expect_true(
    all(file.exists(files))
  )
  
  pkgs <- sapply(strsplit(basename(files), "_"), "[[", 1)
  dupes <- pkgs[duplicated(pkgs)]
  expect_true(
    all(dupes==pkgList[[pkg_type]])
  )
  
})

