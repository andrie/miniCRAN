context("updateRepo")

# make baseline repo ------------------------------------------------------

repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if(file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
dir.create(repo_root, recursive=TRUE, showWarnings = FALSE)

revolution <- c(CRAN="http://mran.revolutionanalytics.com/snapshot/2014-10-15")
pkgs <- c("chron", "acss")

types <- c(source="source", win="win.binary", mac="mac.binary.mavericks")

pdb <- lapply(types, pkgAvail, repos=revolution)
pkgList <- lapply(types, pkgDep, pkg=pkgs, availPkgs=pdb[["source"]], repos=revolution, suggests=FALSE)

miniCRAN:::.copySampleRepo(path=repo_root)

pkgsAdd <- c("foreach")
# revolution <- c(CRAN="http://mran.revolutionanalytics.com/snapshot/2014-12-01")



# Update repo with source and binary packages -----------------------------

test_that("addPackage downloads source files and rebuilds PACKAGES file", {
  
  
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
      pkgListAdd %in% rownames(pkgAvail(repo_root, type=pkg_type))
    )
  )
  
})



test_that("addPackage downloads source files and rebuilds PACKAGES file", {
  
  
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
      pkgListAdd %in% rownames(pkgAvail(repo_root, type=pkg_type))
    )
  )
  
})



test_that("addPackage downloads mac binary files and rebuilds PACKAGES file", {
  
  
  pkg_type <- "mac.binary"
  pkgList  <- pkgList[["mac"]]
  
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
      pkgListAdd %in% rownames(pkgAvail(repo_root, type=pkg_type))
    )
  )
  
})


