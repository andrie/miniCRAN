if (interactive()) {library(testthat); Sys.setenv(NOT_CRAN = "true")}


{
  revolution_mran <- p3m("2024-01-02")
  if (!is.online(revolution_mran, tryHttp = FALSE)) {
    # Use http:// for older versions of R
    revolution_mran <- sub("^https://", "http://", revolution_mran)
  }
  rvers <- "4.0"
  pkgs <- c("MASS")
  repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
  new_repo_root <- file.path(tempdir(), "newMiniCRAN", Sys.Date())
  if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
  if (file.exists(new_repo_root)) unlink(new_repo_root, recursive = TRUE)
  
  # list.files(repo_root, recursive = TRUE)
}

types <- c("win.binary")
names(types) <- c("win.binary")

pkg_type <- names(types)

test_that("makeRepo downloads files and builds PACKAGES file", {
  skip_if_offline(revolution_mran)
  # skip_on_cran()
  for (pkg_type in names(types)) {
    
    # Create local miniCRAN
    # mockery::stub(makeRepo, "download_packages", mock_download_packages, depth = 1)
    # mockery::stub(updateRepoIndex, "write_packages", mock_write_packages, depth = 1)
    
    pdb <- pkgAvail(repos = revolution_mran, type = pkg_type, Rversion = rvers, quiet = TRUE)
    pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = revolution_mran, type = pkg_type,
                      suggests = FALSE, Rversion = rvers)
    prefix <- repoPrefix(pkg_type, Rversion = rvers)
    dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)
    
    ret <- makeRepo(pkgList, path = repo_root, repos = revolution_mran, 
                    type = pkg_type, quiet = TRUE, Rversion = rvers)
    
    expect_type(ret, "character")
    expect_equal(length(ret), length(pkgList))
    
    expect_true(
      .checkForRepoFiles(repo_root, pkgList, prefix)
    )
    expect_true(
      file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
    )
    expect_true(
      all(
        pkgList %in% pkgAvail(repos = repo_root, type = pkg_type, Rversion = rvers)[, "Package"]
      )
    )
    
    # Create miniCRAN from existing miniCRAN
    
    localCRAN <- paste0("file:///", repo_root)
    
    pdb <- pkgAvail(repos = localCRAN, type = pkg_type, Rversion = rvers)
    pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = localCRAN, type = pkg_type,
                      suggests = FALSE, Rversion = rvers)
    prefix <- repoPrefix(pkg_type, Rversion = rvers)
    dir.create(new_repo_root, recursive = TRUE, showWarnings = FALSE)
    
    ret <- makeRepo(pkgList, path = new_repo_root, repos = localCRAN, 
                    type = pkg_type, quiet = TRUE, Rversion = rvers)
    
    expect_type(ret, "character")
    expect_equal(length(ret), length(pkgList))
    
    expect_true(
      .checkForRepoFiles(new_repo_root, pkgList, prefix)
    )
    expect_true(
      file.exists(file.path(new_repo_root, prefix, "PACKAGES.gz"))
    )
    expect_true(
      all(
        pkgList %in% pkgAvail(repos = new_repo_root, type = pkg_type, Rversion = rvers)[, "Package"]
      )
    )
    
  }
})

unlink(repo_root, recursive = TRUE)
