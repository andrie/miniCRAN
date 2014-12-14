context("makeRepo")


revolution <- c(CRAN="http://cran.revolutionanalytics.com")
pkgs <- c("foreach")
repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
if(file.exists(repo_root)) unlink(repo_root, recursive = TRUE)

# list.files(repo_root, recursive = TRUE)


# source ------------------------------------------------------------------

test_that("makeRepo downloads source files and builds PACKAGES file", {

  skip_on_cran()

  pkg_type <- "source"
  pdb <- pkgAvail(repos = revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, R.version)
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

  makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)

  expect_true(
    miniCRAN:::.checkForRepoFiles(repo_root, pkgList, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  expect_true(
    all(
      pkgList %in% pkgAvail(repo_root, type=pkg_type)[, "Package"]
    )
  )
  
})

# windows binaries --------------------------------------------------------

test_that("makeRepo downloads windows binary files and builds PACKAGES file", {

  pkg_type <- "win.binary"
  pdb <- pkgAvail(repos = revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, miniCRAN:::twodigitRversion(R.version))
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

  makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  list.files(repo_root, recursive = TRUE)
  expect_true(
    miniCRAN:::.checkForRepoFiles(repo_root, pkgList, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  expect_true(
    all(
      pkgList %in% pkgAvail(repo_root, type=pkg_type)[, "Package"]
    )
  )
  
})

# mac binaries ------------------------------------------------------------

test_that("makeRepo downloads mac binary files and builds PACKAGES file", {

  skip_on_cran()

  pkg_type <- "mac.binary"
  pdb <- pkgAvail(repos = revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, miniCRAN:::twodigitRversion(R.version))
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

  makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  list.files(repo_root, recursive = TRUE)
  expect_true(
    miniCRAN:::.checkForRepoFiles(repo_root, pkgList, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  expect_true(
    all(
      pkgList %in% pkgAvail(repo_root, type=pkg_type)[, "Package"]
    )
  )
  
})

# mac mavericks binaries --------------------------------------------------

test_that("makeRepo downloads mac mavericks binary files and builds PACKAGES file", {

  pkg_type <- "mac.binary.mavericks"
  pdb <- pkgAvail(repos = revolution, type=pkg_type)
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos=revolution, type=pkg_type, suggests=FALSE)
  prefix <- miniCRAN:::repoPrefix(pkg_type, miniCRAN:::twodigitRversion(R.version))
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)
  
  makeRepo(pkgList, path=repo_root, repos=revolution, type=pkg_type, quiet=TRUE)
  list.files(repo_root, recursive = TRUE)
  expect_true(
    miniCRAN:::.checkForRepoFiles(repo_root, pkgList, prefix)
  )
  expect_true(
    file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
  )
  expect_true(
    all(
      pkgList %in% pkgAvail(repo_root, type=pkg_type)[, "Package"]
    )
  )
  
})

unlink(repo_root, recursive = TRUE)

