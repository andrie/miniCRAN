# These functions mock downloading of packages, for fast testing purposes only

is_mock_environment <- function() Sys.getenv("miniCRAN.mock.download") == TRUE
set_mock_environment <- function() Sys.setenv("miniCRAN.mock.download" = TRUE)
reset_mock_environment <- function() Sys.setenv("miniCRAN.mock.download" = FALSE)


# lib_in_tempdir <- function(lib) {
#   np <- function(x)normalizePath(x, winslash = "/")
#   grepl(np(tempdir()), np(lib))
# }

download.packages <- function(...) {
  if (is_mock_environment()) {
    mock.download.packages(...)
  } else {
    utils::download.packages(...)
  }
}

write_packages <- function(dir, type) {
  if (is_mock_environment()) {
    mock.write_packages(dir = dir, type = type)
  } else {
    tools::write_PACKAGES(dir = dir, type = type)
  }
}

#' @importFrom utils available.packages
mock.download.packages <- function(pkgs, destdir, available, type, ...) {
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

mock.write_packages <- function(dir, type = "source") {
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
    saveRDS(db, file.path(dir, "PACKAGES.rds"), compress = "xz")
  }
  np
}



# mock.updateRepoIndex <- function(path, type = "source", Rversion = R.version) {
#   do_one <- function(t) {
#     pkg_path <- repoBinPath(path = path, type = t, Rversion = Rversion)
#     pattern <- ".tgz$|.zip$|.tar.gz$"
#     if (grepl("mac.binary", t)) t <- "mac.binary"
#     ff <- list.files(pkg_path, recursive = TRUE, full.names = TRUE, pattern = pattern)
#     ffb <- basename(ff)
#     pkgs <- ffb[!grepl("^PACKAGES.*", ffb)]
#     np <- length(pkgs)
#     pkg_names <- gsub(pattern, "", pkgs)
#     db <- matrix(unlist(strsplit(pkg_names, "_")), ncol = 2, byrow = TRUE)
#     colnames(db) <- c("Package", "Version")
#     db
#
#     if (np > 0L) {
#       db[!is.na(db) & (db == "")] <- NA_character_
#       con <- file(file.path(pkg_path, "PACKAGES"), "wt")
#       write.dcf(db, con)
#       close(con)
#       con <- gzfile(file.path(pkg_path, "PACKAGES.gz"), "wt")
#       write.dcf(db, con)
#       close(con)
#       rownames(db) <- db[, "Package"]
#       saveRDS(db, file.path(pkg_path, "PACKAGES.rds"), compress = "xz")
#     }
#     np
#   }
#   n <- sapply(type, do_one, USE.NAMES = TRUE, simplify = FALSE)
#   names(n) <- type
#   n
# }
#
