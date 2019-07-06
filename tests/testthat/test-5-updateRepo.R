if (interactive()) {library(testthat); Sys.setenv(NOT_CRAN = "true")}
# set_mock_environment()

context("updateRepo")

# make baseline repo ------------------------------------------------------

{
  repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)
  
  revolution <- MRAN("2014-10-15")
  if (!is.online(revolution, tryHttp = FALSE)) {
    # Use http:// for older versions of R
    revolution <- sub("^https://", "http://", revolution)
  }
  rvers <- "3.1"
  pkgs <- c("chron", "adaptivetau")
  
  types <- c("win.binary", "mac.binary", "source")
  # types <- c("win.binary")
  
  names(types) <- types
  pdb <- list()
  pkgList <- list()
}

test_that("sample repo is setup correctly", {
  skip_if_offline(revolution)
  
  pdb <<- lapply(types, pkgAvail, repos = revolution, Rversion = rvers, quiet = TRUE)
  expect_is(pdb, "list")
  pkgList <<- lapply(types, function(type) {
    pkgDep(pkg = pkgs, type = types[type], availPkgs = pdb[[type]],
           repos = revolution, suggests = FALSE, Rversion = rvers)
  })
  expect_is(pkgList, "list")
  
  z <- .createSampleRepo(path = repo_root, MRAN = revolution, Rversion = rvers)
  expect_is(z, "character")
  expect_equal(unname(pkgAvail(repo_root, quiet = TRUE)[, "Package"]), sort(pkgs))
})


# Add packages to repo ----------------------------------------------------

pkgsAdd <- c("forecast")

pkg_type <- names(types)[1]
for (pkg_type in names(types)) {
  
  context(sprintf(" - Add packages to repo (%s)", pkg_type))
  
  test_that(sprintf(
    "addPackage downloads %s files and rebuilds PACKAGES file", 
    pkg_type), {
      
      skip_on_cran()
      skip_if_offline(revolution)
      
      pkgListAdd <- pkgDep(pkgsAdd, availPkgs = pdb[[pkg_type]],
                           repos = revolution,
                           type  = pkg_type,
                           suggests = FALSE,
                           Rversion = rvers)
      prefix <- repoPrefix(pkg_type, Rversion = rvers)
      
      
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          addPackage(pkgListAdd, path = repo_root, repos = revolution, type = pkg_type,
                 quiet = TRUE, Rversion = rvers)
        })

      expect_true(
        .checkForRepoFiles(repo_root, pkgListAdd, prefix)
      )
      expect_true(
        file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
      )
      expect_true(
        all(
          pkgListAdd %in% pkgAvail(repo_root, 
                                   type = pkg_type, 
                                   Rversion = rvers,
                                   quiet = TRUE)[, "Package"]
        )
      )
    })
}


# Add local packages to repo ----------------------------------------------

pkgsAddLocal <- c("MASS")

for (pkg_type in names(types)) {
  
  context(sprintf(" - Add local packages to repo (%s)", pkg_type))
  
  test_that(
    sprintf("addLocalPackage copies %s files and rebuilds PACKAGES", 
            pkg_type), 
    {
      
      skip_on_cran()
      skip_if_offline(revolution)
      
      tmpdir <- file.path(tempdir(), "miniCRAN", "local", pkg_type)
      expect_true(dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE))
      tmpdir <- normalizePath(tmpdir)
      expect_true(dir.exists(tmpdir))
      on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
      
      # get most recent version
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          
          res <- download_packages(
            pkgsAddLocal, destdir = tmpdir, 
            type = pkg_type,
            available = pkgAvail(revolution, pkg_type, rvers),
            contriburl = contribUrl(revolution, pkg_type, rvers),
            quiet = TRUE)
        })
      
      # simulate older version also present in pkgPath directory
      f <- res[, 2]
      expect_true(
        file.copy(from = f, to = file.path(tmpdir, "MASS_7.3-0.tar.gz"))
      )
      expect_equal(length(list.files(tmpdir)), 2)
      
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          addLocalPackage(pkgs = pkgsAddLocal, pkgPath = tmpdir, path = repo_root,
                      type = pkg_type, quiet = TRUE, Rversion = rvers)
        })

      prefix <- repoPrefix(pkg_type, Rversion = rvers)
      expect_true(
        .checkForRepoFiles(repo_root, pkgsAddLocal, prefix)
      )
      expect_true(
        file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
      )
      expect_true(
        all(
          pkgsAddLocal %in% pkgAvail(repo_root, type = pkg_type, 
                                     Rversion = rvers)[, "Package"]
        )
      )
    })
}


# Check for updates -------------------------------------------------------


MRAN_mirror <- MRAN("2015-01-01")
if (!is.online(MRAN_mirror, tryHttp = FALSE)) {
  # Use http:// for older versions of R
  MRAN_mirror <- sub("^https://", "http://", revolution)
}

pkg_type <- names(types)[1]
for (pkg_type in names(types)) {
  context(sprintf(" - Check for updates (%s)", pkg_type))
  
  test_that(
    sprintf("updatePackages downloads %s files and builds PACKAGES", pkg_type), 
    {
      
      skip_on_cran()
      skip_if_offline(MRAN_mirror)

      prefix <- repoPrefix(pkg_type, Rversion = rvers)
      
      suppressWarnings(
        old <- oldPackages(path = repo_root, repos = MRAN_mirror, 
                           type = pkg_type, Rversion = rvers,
                           quiet = FALSE)
      )
      
      # In the following allow for differences between mac.binary and other types
      expect_true(nrow(old) >= 10)
      expect_true(nrow(old) <= 12)
      expect_equal(ncol(old), 4)
      expect_true(
        all(
          rownames(old) %in% 
            c("adaptivetau", "BH", "digest", "forecast", "Hmisc", "mvtnorm", 
              "RColorBrewer", "RcppArmadillo", "reshape2", "timeDate", 
              "timeSeries", "tis")
        )
      )
     
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          updatePackages(path = repo_root, repos = MRAN_mirror, type = pkg_type,
                         ask = FALSE, quiet = TRUE, Rversion = rvers)
        })

      updateVers <- getPkgVersFromFile(
        list.files(file.path(repo_root, prefix))
      )
      
      expect_true(
        .checkForRepoFiles(repo_root, pkgList[[pkg_type]], prefix)
      )
      
      expect_true(
        file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
      )
      
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          old <- oldPackages(path = repo_root, repos = MRAN_mirror, 
                             type = pkg_type, Rversion = rvers)
        })
      # browser()
      expect_equal(nrow(old), 0)
      expect_equal(ncol(old), 4)
      
    })
}


# Check for duplicate packages --------------------------------------------

context(" - Check for duplicate files")

for (pkg_type in names(types)) {
  
  test_that(
    sprintf("checkVersions() finds out-of-date %s packages", pkg_type), 
    {
      
      skip_on_cran()
      skip_if_offline(MRAN_mirror)
          
      
          oldVersions <- list(package = c("acepack"),
                              version = c("1.3-2"))
          
          if (pkg_type != "source") {
            
            expect_error(
              with_mock(
                download_packages = mock_download_packages,
                write_packages = mock_write_packages,
                .env = "miniCRAN",
                {
                  addOldPackage(oldVersions[["package"]], path = repo_root, 
                                vers = oldVersions[["version"]],
                                repos = MRAN_mirror, type = pkg_type)
                })
            )
          } else {
            with_mock(
              download_packages = mock_download_packages,
              write_packages = mock_write_packages,
              .env = "miniCRAN",
              {
                addOldPackage(oldVersions[["package"]], path = repo_root, 
                              vers = oldVersions[["version"]],
                              repos = MRAN_mirror, type = pkg_type)
              })
            files <- suppressWarnings(
              checkVersions(path = repo_root, type = pkg_type)[[pkg_type]]
            )
            
            expect_true(
              all(file.exists(files))
            )
            
            pkgs <- sapply(strsplit(basename(files), "_"), "[[", 1)
            dupes <- pkgs[duplicated(pkgs)]
            expect_true(
              all(dupes == oldVersions[["package"]])
            )
            
          }
    })
}
