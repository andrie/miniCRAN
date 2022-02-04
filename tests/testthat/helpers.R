# helper functions for testing

# Returns TRUE if a URL can be accessed
is.online <- function(url = MRAN(), tryHttp = TRUE) {
  readFromUrl <- function(url){
    z <- tryCatch(suppressWarnings(readLines(url, n = 1, warn = FALSE)),
                  error = function(e) e
    )
    !inherits(z, "error")
  }

  if (!readFromUrl(url)) {
    if (grepl("^https://", url) && tryHttp) {
      url <- sub("^https://", "http://", url) # for older versions of R
      if (!readFromUrl(url))
        return(FALSE)
    } else {
      return(FALSE)
    }
  }

  TRUE
}

# Interrupt the test if url can not be reached
skip_if_offline <- function(url = MRAN()) {
  if (!is.online(url)) testthat::skip("offline")
}


set_test_types <- function() {
  types <- 
    Sys.getenv(
      "minicran_test_scope",
      unset = "source, win.binary, mac.binary, mac.binary.mavericks"
    )
  
  types <- gsub(" +", ",", types)
  strsplit(types, ",")[[1]]
}


# Use in unit tests to check if source or binary files are in repo
.checkForRepoFiles <- function(path, pkgList, prefix) {
  ptn <- "tar\\.gz|zip|tgz"
  ff <- list.files(file.path(path, prefix), recursive = TRUE, pattern = ptn)
  if (length(ff) < length(pkgList)) return(FALSE)
  ret <- sapply(pkgList, function(x)any(grepl(x, ff)))
  if (all(ret > 0)) TRUE else {
    message(ret)
    FALSE
  }
}

# These functions mock downloading of packages, for fast testing purposes only


#' @importFrom utils available.packages
mock_download_packages <- function(pkgs, destdir, available, type, ...) {
  if (missing(available) || is.null(available)) available <- available.packages()
  downloadFileName <- function(package, version, type) {
    paste0(package, "_", version, pkgFileExt(type))
  }
  versions <- setNames(available[pkgs, "Version"], pkgs)
  downloads <- mapply(names(versions), versions,
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
      saveRDS(db, file.path(dir, "PACKAGES.rds"), compress = "xz")
    } else {
      saveRDS(db, file.path(dir, "PACKAGES.rds"), compress = "xz", version = 2)
    }
  }
  np
}


# Create sample repo from MRAN snapshot
.createSampleRepo <- function(MRAN, path, pkgs, Rversion = "3.1") {
  if (missing(MRAN)) MRAN <- MRAN("2014-10-15")
  if (missing(path)) path <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (missing(pkgs)) pkgs <- c("chron", "adaptivetau")

  pdb_source <- pkgAvail(repos = MRAN, type = "source", Rversion = Rversion)
  pdb_win    <- pkgAvail(repos = MRAN, type = "win.binary", Rversion = Rversion)
  pdb_mac    <- pkgAvail(repos = MRAN, type = "mac.binary", Rversion = Rversion)

  pkgList_source <- pkgDep(pkgs, availPkgs = pdb_source, repos = MRAN,
                           type = "source", suggests = FALSE, Rversion = Rversion)
  
  makeRepo(pkgList_source, path = path, repos = MRAN,
           type = "source",
           quiet = TRUE, Rversion = Rversion)
  
  pkgList_win <- pkgDep(pkgs, availPkgs = pdb_win, repos = MRAN,
                        type = "win.binary",
                        suggests = FALSE, Rversion = Rversion)
  makeRepo(pkgList_win, path = path, repos = MRAN,
           type = "win.binary",
           quiet = TRUE, Rversion = Rversion)
  
  pkgList_mac <- pkgDep(pkgs, availPkgs = pdb_mac, repos = MRAN,
                        type = "mac.binary",
                        suggests = FALSE, Rversion = Rversion)
  makeRepo(pkgList_mac, path = path, repos = MRAN,
           type = "mac.binary",
           quiet = TRUE, Rversion = Rversion)
}

make_fake_package <- function(version = "0.1.0", base_path = tempdir()) {
  fake_package <- file.path(base_path, "fake.package")
  dir.create(fake_package, showWarnings = FALSE)
  
  # Create a fake function to add to the package
  foo <- function(x)NA
  
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
  
  ")
  
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
