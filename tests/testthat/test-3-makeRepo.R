

{
  revolution <- MRAN("2024-01-02")
  if (!is.online(revolution, tryHttp = FALSE)) {
    # Use http:// for older versions of R
    revolution <- sub("^https://", "http://", revolution)
  }
  rvers <- "4.0"
  pkgs <- c("Rcpp")
  repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
  
  # list.files(repo_root, recursive = TRUE)
  
  unlink(list.files(tempdir(), pattern = ".rds$", full.names = TRUE))
  
}

# types are defined in env var `minicran_test_scope`
types <- set_test_types()

test_for_type <- function(pkg_type) {
  # skip_if_not_installed("mockr") 
  test_that(sprintf("makeRepo downloads %s files and builds PACKAGES",
                    pkg_type), {

    skip_on_cran()
    skip_if_offline()
    skip_if_not_installed("mockr")

    pdb <- pkgAvail(repos = revolution, type = pkg_type, Rversion = rvers, quiet = TRUE)
    pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = revolution, type = pkg_type,
                      suggests = FALSE, Rversion = rvers, quiet = FALSE)
    prefix <- repoPrefix(pkg_type, Rversion = rvers)
    dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

    mockr::with_mock(
      download_packages = mock_download_packages,
      write_packages = mock_write_packages,
      .env = "miniCRAN",
      {
        ret <- makeRepo(pkgList, path = repo_root, repos = revolution,
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
  })
}

for (pkg_type in (types)) {test_for_type(pkg_type = pkg_type)}

unlink(repo_root, recursive = TRUE)
