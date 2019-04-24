if (interactive()) {library(testthat); Sys.setenv(NOT_CRAN = "true")}
# set_mock_environment()

context("deleteRepo")

# make baseline repo ------------------------------------------------------

{
  repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

  revolution <- MRAN("2019-04-01")
  if (!is.online(revolution, tryHttp = FALSE)) {
    # Use http:// for older versions of R
    revolution <- sub("^https://", "http://", revolution)
  }
  rvers <- "3.5"

  # types <- c("win.binary", "mac.binary", "source")
  types <- c("win.binary", "source")

  names(types) <- types
  pdb <- list()
  pkgList <- list()
}

# Delete packages from repo ----------------------------------------------------

# LVGP imports randtoolbox, which depends on rngWELL
# LVGP imports lhs, which imports Rcpp
pkgsAdd <- c("LVGP")

pkg_type <- names(types)[1]
for (pkg_type in names(types)) {

  context(sprintf(" - Delete packages from repo (%s)", pkg_type))

  test_that(sprintf(
    "deletePackage deletes %s files",
    pkg_type), {

      skip_on_cran()
      skip_if_offline(revolution)

      pdb <<- lapply(types, pkgAvail, repos = revolution, Rversion = rvers, quiet = TRUE)
      pkgListAdd <- pkgDep(pkgsAdd, availPkgs = pdb[[pkg_type]],
                           repos = revolution,
                           type  = pkg_type,
                           suggests = FALSE,
                           Rversion = rvers)

      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          addPackage(pkgListAdd, path = repo_root, repos = revolution, type = pkg_type,
                     quiet = TRUE, Rversion = rvers)
        })

      expect_true(
        all(
          pkgListAdd %in% pkgAvail(repo_root,
                                   type = pkg_type,
                                   Rversion = rvers,
                                   quiet = TRUE)[, "Package"]
        )
      )

      # Delete only listed packages, leave all deps as is
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          deletePackage(pkgsAdd, path = repo_root, type = pkg_type, Rversion = rvers, deps = FALSE)
          pdb <- pkgAvail(repo_root,
                          type = pkg_type,
                          Rversion = rvers,
                          quiet = TRUE)[, "Package"]
        })

      expect_true(
        all(
          c(!(pkgsAdd %in% pdb), setdiff(pkgListAdd, pkgsAdd) %in% pdb)
        )
      )

      # TODO: more tests
    })
}
