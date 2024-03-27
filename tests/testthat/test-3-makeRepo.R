

{
  revolution <- p3m("2023-08-31")
  if (!is.online(revolution, tryHttp = FALSE)) {
    # Use http:// for older versions of R
    revolution <- sub("^https://", "http://", revolution)
  }
  rvers <- "4.3"
  pkgs <- c("Rcpp")
  repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
  
  # list.files(repo_root, recursive = TRUE)
  
  unlink(list.files(tempdir(), pattern = ".rds$", full.names = TRUE))
  
}

# types are defined in env var `minicran_test_scope`
types <- set_test_types()
pkg_type <- types[1]

test_that("makeRepo downloads files and builds PACKAGES", {
  
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("mockr")
  types <- set_test_types()
  for (pkg_type in types) {
    
    # pdb <- pkgAvail(repos = revolution, type = pkg_type, quiet = TRUE)
    pdb <- pkgAvail(repos = revolution, type = pkg_type, Rversion = rvers, quiet = TRUE)
    pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = revolution, type = pkg_type,
      suggests = FALSE, Rversion = rvers, quiet = FALSE)
      prefix <- repoPrefix(pkg_type, Rversion = rvers)
      dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)
      
      ret <- mockr::with_mock(
    download_packages = mock_download_packages,
    write_packages = mock_write_packages,
    .env = "miniCRAN",
    {
      makeRepo(pkgList, path = repo_root, repos = revolution,
                type = pkg_type, quiet = TRUE, Rversion = rvers)
    }
  )
  
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
      pkgList %in% pkgAvail(repos = repo_root, 
        type = pkg_type, 
        Rversion = rvers)[, "Package"]
      )
    )
  }
})



# types are defined in env var `minicran_test_scope`

unlink(repo_root, recursive = TRUE)
