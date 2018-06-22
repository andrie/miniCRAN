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

mock_write_packages <- function(dir, type = "source") {
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
