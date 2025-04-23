# helper functions for testing

hostname <- function(x) {
  y <- sub("https*://", "", x)
  sub("latest$", "", y)
}


# Interrupt the test if url can not be reached
skip_if_offline <- function(url = p3m()) {
  # browser()
  mran_host <- hostname(p3m())
  if (is.null(url)) url <- mran_host
  if (!is.online(url = url)) testthat::skip("offline")
}


set_test_types <- function() {
  types <-
    Sys.getenv(
      "minicran_test_scope",
      unset = "source, win.binary, mac.binary"
    )

  types <- gsub(" +", "", types)
  unname(strsplit(types, ",")[[1]])
}


# Use in unit tests to check if source or binary files are in repo
.checkForRepoFiles <- function(path, pkgList, prefix) {
  ptn <- "tar\\.gz|zip|tgz"
  ff <- list.files(file.path(path, prefix), recursive = TRUE, pattern = ptn)
  if (length(ff) < length(pkgList)) return(FALSE)
  ret <- sapply(pkgList, function(x) any(grepl(x, ff)))
  if (all(ret > 0)) TRUE else {
    message(ret)
    FALSE
  }
}

# These functions mock downloading of packages, for fast testing purposes only

#' @importFrom utils available.packages
mock_download_packages <- function(pkgs, destdir, available, type, ...) {
  if (missing(available) || is.null(available))
    available <- available.packages()
  downloadFileName <- function(package, version, type) {
    paste0(package, "_", version, pkgFileExt(type))
  }
  versions <- setNames(available[pkgs, "Version"], pkgs)
  downloads <- mapply(
    names(versions),
    versions,
    USE.NAMES = FALSE,
    FUN = function(p, v) {
      fn <- file.path(destdir, downloadFileName(p, v, type))
      writeLines("", fn)
      matrix(c(p, fn), ncol = 2)
    }
  )
  t(downloads)
}

mock_write_packages <- function(dir, type = "source", r_version) {
  pattern <- ".tgz$|.zip$|.tar.gz$"
  if (grepl("mac.binary", type)) type <- "mac.binary"
  ff <- list.files(dir, recursive = TRUE, full.names = TRUE, pattern = pattern)
  ffb <- basename(ff)
  pkgs <- ffb[!grepl("^PACKAGES.*", ffb)]
  np <- length(pkgs)
  pkg_names <- gsub(pattern, "", pkgs)
  db <- matrix(unlist(strsplit(pkg_names, "_")), ncol = 2, byrow = TRUE)
  colnames(db) <- c("Package", "Version")
  db

  if (np > 0L) {
    db[!is.na(db) & (db == "")] <- NA_character_
    con <- file(file.path(dir, "PACKAGES"), "wt")
    write.dcf(db, con)
    close(con)
    con <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
    write.dcf(db, con)
    close(con)
    rownames(db) <- db[, "Package"]
    r_version <- twodigitRversion(r_version)
    if (r_version >= "3.5.0") {
      saveRDS(db, file.path(dir, "PACKAGES.rds"))
    } else {
      saveRDS(db, file.path(dir, "PACKAGES.rds"), version = 2)
    }
  }
  np
}


# Create sample repo from p3m snapshot
.createSampleRepo <- function(p3m, path, pkgs, Rversion = "4.0", types) {
  if (missing(p3m)) p3m <- p3m("2024-01-02")
  if (missing(path)) path <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (missing(pkgs)) pkgs <- c("chron", "curl")
  if (missing(types)) types <- set_test_types()

  # pdb_source <- pkgAvail(repos = p3m, type = "source", Rversion = Rversion)
  # pdb_win    <- pkgAvail(repos = p3m, type = "win.binary", Rversion = Rversion)
  # pdb_mac    <- pkgAvail(repos = p3m, type = "mac.binary", Rversion = Rversion)

  mockr::with_mock(
    download_packages = mock_download_packages,
    write_packages = mock_write_packages,
    .env = "miniCRAN",
    {
      for (type in types) {
        pdb <- pkgAvail(repos = p3m, type = type, Rversion = Rversion)
        pkgList_source <- pkgDep(
          pkgs,
          availPkgs = pdb,
          repos = p3m,
          type = type,
          suggests = FALSE,
          Rversion = Rversion
        )

        makeRepo(
          pkgList_source,
          path = path,
          repos = p3m,
          type = type,
          quiet = TRUE,
          Rversion = Rversion
        )
      }
    }
  )
}

make_fake_package <- function(version = "0.1.0", base_path = tempdir()) {
  fake_package <- file.path(base_path, "fake.package")
  dir.create(fake_package, showWarnings = FALSE)

  # Create a fake function to add to the package
  foo <- function(x) NA

  # Create the skeleton

  # browser()

  if (getRversion() >= "3.5") {
    suppressMessages(
      package.skeleton(
        "fake.package",
        path = base_path,
        list = "foo",
        force = TRUE,
        environment = environment(foo),
        encoding = "UTF-8"
      )
    )
  } else {
    suppressMessages(
      package.skeleton(
        "fake.package",
        path = base_path,
        list = "foo",
        force = TRUE,
        environment = environment(foo)
      )
    )
  }

  # Remove unnecessary detritus from skeleton
  file.remove(file.path(fake_package, "NAMESPACE"))
  unlink(file.path(fake_package, "data"), recursive = TRUE)
  unlink(file.path(fake_package, "man"), recursive = TRUE)
  unlink(file.path(fake_package, "Read-and-delete-me"), recursive = TRUE)

  # Write a function file with some roxygen
  writeLines(
    con = file.path(fake_package, "R", "foo.R"),
    text = "
  #' Foo.
  #' 
  #' Does nothing.
  #' @export
  #' foo <- function(x)NULL
  
  "
  )

  # Set package version
  desc <- readLines(file.path(fake_package, "DESCRIPTION"))
  version_line <- grep("^Version:", desc)
  desc[version_line] <- paste0("Version: ", version)
  writeLines(desc, con = file.path(file.path(fake_package, "DESCRIPTION")))

  # Document the package

  suppressMessages(
    devtools::document(fake_package, quiet = TRUE)
  )

  # Build the package
  devtools::build(fake_package, path = base_path, quiet = TRUE)
}
