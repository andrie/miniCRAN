# make baseline repo ------------------------------------------------------

{
  repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

  mirror <- p3m("2023-08-31")
  if (!is.online(mirror, tryHttp = FALSE)) {
    # Use http:// for older versions of R
    mirror <- sub("^https://", "http://", mirror)
  }
  rvers <- "4.2"
  pkgs <- c("chron", "data.table")

  types <- intersect(
    set_test_types(),
    # c("source", "win.binary")
    c("source", "win.binary", "mac.binary")
  )

  names(types) <- types
  pdb <- list()
  pkgList <- list()
}

test_that("sample repo is setup correctly", {
  skip_if_offline(mirror)

  type <- types[1]
  pdb <<- lapply(
    types,
    pkgAvail,
    repos = mirror,
    Rversion = rvers,
    quiet = TRUE
  )
  expect_type(pdb, "list")
  pkgList <<- lapply(types, function(type) {
    # type <- names(type)
    pkgDep(
      pkg = pkgs,
      type = names(type),
      availPkgs = pdb[[type]],
      repos = mirror,
      suggests = FALSE,
      Rversion = rvers
    )
  })
  expect_type(pkgList, "list")

  .createSampleRepo(
    path = repo_root,
    p3m = mirror,
    Rversion = rvers,
    pkgs = pkgs,
    types = names(types)
  )
  # expect_type(z, "character")
  pkg_names <- unname(pkgAvail(
    repo_root,
    quiet = TRUE,
    type = type,
    Rversion = rvers
  )[, "Package"])
  expect_true(all(pkgs %in% pkg_names))
})


# Add packages to repo ----------------------------------------------------

pkgsAdd <- c("Rcpp")
pkg_type <- names(types)[1]

for (pkg_type in names(types)) {
  skip_if_not_installed("mockr")

  test_that(
    sprintf(
      "addPackage downloads %s files and rebuilds PACKAGES file",
      pkg_type
    ),
    {
      skip_on_cran()
      skip_if_offline(mirror)

      pkgListAdd <- pkgDep(
        pkgsAdd,
        availPkgs = pdb[[pkg_type]],
        repos = mirror,
        type = pkg_type,
        suggests = FALSE,
        Rversion = rvers
      )
      prefix <- repoPrefix(pkg_type, Rversion = rvers)

      mockr::with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          addPackage(
            pkgListAdd,
            path = repo_root,
            repos = mirror,
            type = pkg_type,
            quiet = TRUE,
            Rversion = rvers
          )
        }
      )

      expect_true(
        .checkForRepoFiles(repo_root, pkgListAdd, prefix)
      )
      expect_true(
        file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
      )
      expect_true(
        all(
          pkgListAdd %in%
            pkgAvail(
              repo_root,
              type = pkg_type,
              Rversion = rvers,
              quiet = TRUE
            )[, "Package"]
        )
      )
    }
  )
}


# Add local packages to repo ----------------------------------------------

pkgsAddLocal <- c("nnet")

for (pkg_type in names(types)) {
  skip_if_not_installed("mockr")
  test_that(
    sprintf("addLocalPackage copies %s files and rebuilds PACKAGES", pkg_type),
    {
      skip_on_cran()
      skip_if_offline(mirror)

      tmpdir <- file.path(tempdir(), "miniCRAN", "local", pkg_type)
      expect_true(dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE))

      tmpdir <- normalizePath(tmpdir)
      expect_true(dir.exists(tmpdir))
      on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

      # get most recent version
      mockr::with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          res <- download_packages(
            pkgsAddLocal,
            destdir = tmpdir,
            type = pkg_type,
            available = pkgAvail(mirror, pkg_type, rvers),
            contriburl = contribUrl(mirror, pkg_type, rvers),
            quiet = TRUE
          )
        }
      )

      # simulate older version also present in pkgPath directory
      f <- res[, 2]
      expect_true(
        file.copy(from = f, to = file.path(tmpdir, "MASS_7.3-0.tar.gz"))
      )
      expect_equal(length(list.files(tmpdir)), 2)

      mockr::with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          addLocalPackage(
            pkgs = pkgsAddLocal,
            pkgPath = tmpdir,
            path = repo_root,
            type = pkg_type,
            quiet = TRUE,
            Rversion = rvers
          )
        }
      )

      prefix <- repoPrefix(pkg_type, Rversion = rvers)
      expect_true(
        .checkForRepoFiles(repo_root, pkgsAddLocal, prefix)
      )
      expect_true(
        file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
      )
      expect_true(
        all(
          pkgsAddLocal %in%
            pkgAvail(repo_root, type = pkg_type, Rversion = rvers)[, "Package"]
        )
      )
    }
  )
}


# Check for updates -------------------------------------------------------

p3m_mirror <- p3m("2024-01-02")
if (!is.online(p3m_mirror, tryHttp = FALSE)) {
  # Use http:// for older versions of R
  p3m_mirror <- sub("^https://", "http://", mirror)
}

pkg_type <- names(types)[1]

for (pkg_type in names(types)) {
  test_that(
    sprintf("updatePackages downloads %s files and builds PACKAGES", pkg_type),
    {
      skip_on_cran()
      skip_if_offline(p3m_mirror)

      prefix <- repoPrefix(pkg_type, Rversion = rvers)

      old <- suppressWarnings(
        oldPackages(
          path = repo_root,
          repos = p3m_mirror,
          type = pkg_type,
          Rversion = rvers,
          quiet = FALSE
        )
      )

      # In the following allow for differences between mac.binary and other types
      expect_gte(nrow(old), 1)
      expect_equal(ncol(old), 4)
      expect_true("data.table" %in% rownames(old))

      mockr::with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          updatePackages(
            path = repo_root,
            repos = p3m_mirror,
            type = pkg_type,
            ask = FALSE,
            quiet = TRUE,
            Rversion = rvers
          )
        }
      )

      updateVers <- getPkgVersFromFile(
        list.files(file.path(repo_root, prefix))
      )

      expect_true(
        .checkForRepoFiles(repo_root, pkgList[[pkg_type]], prefix)
      )

      expect_true(
        file.exists(file.path(repo_root, prefix, "PACKAGES.gz"))
      )

      mockr::with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          old <- oldPackages(
            path = repo_root,
            repos = p3m_mirror,
            type = pkg_type,
            Rversion = rvers
          )
        }
      )
      # browser()
      expect_equal(nrow(old), 0)
      expect_equal(ncol(old), 4)
    }
  )
}


# Check for duplicate packages --------------------------------------------

pkg_type <- names(types)[3]
for (pkg_type in names(types)) {
  test_that(
    sprintf("checkVersions() finds out-of-date %s packages", pkg_type),
    {
      skip_on_cran()
      skip_if_offline(p3m_mirror)

      oldVersions <- list(package = c("acepack"), version = c("1.3-2"))

      if (pkg_type != "source") {
        expect_error(
          mockr::with_mock(
            download_packages = mock_download_packages,
            write_packages = mock_write_packages,
            .env = "miniCRAN",
            {
              addOldPackage(
                oldVersions[["package"]],
                path = repo_root,
                vers = oldVersions[["version"]],
                repos = p3m_mirror,
                type = pkg_type
              )
            }
          )
        )
      } else {
        mockr::with_mock(
          download_packages = mock_download_packages,
          write_packages = mock_write_packages,
          .env = "miniCRAN",
          {
            addOldPackage(
              oldVersions[["package"]],
              path = repo_root,
              vers = oldVersions[["version"]],
              repos = p3m_mirror,
              type = pkg_type
            )
          }
        )
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
    }
  )
}
