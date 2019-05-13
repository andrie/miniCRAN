#' Delete packages from a miniCRAN repository.
#' @inheritParams addPackage
#' @param deps logical indicating whether the package reverse dependencies should be removed (default `TRUE`).
#' @param ... arguments passed over to `tools::package_dependencies`: use `which`, `recursive`, `reverse`
#' to specify what should happen to dependencies of removed packages
#' @export
#' @family update repo functions
deletePackage <- function(pkgs = NULL, path = NULL,
                          type = "source", Rversion = R.version,
                          writePACKAGES = TRUE, deps = TRUE, ...) {
  if (is.null(path) || is.null(pkgs)) stop("path and pkgs must both be specified.")

  sapply(type, function(t) {
    repoPath <- file.path(path, repoPrefix(t, Rversion))
    db <- pkgAvail(repos = path, type = t, Rversion = Rversion)
    purgePackage(pkgs, db, t, repoPath)

    if (deps) {
      d <- tools::package_dependencies(db = db, ...)
      depends <- unique(unlist(lapply(pkgs, function(pkg) {
        if (!is.null(d[[pkg]])) d[[pkg]] else character()
      })))
      l <- list(...)
      if ('reverse' %in% names(l)) {
        # Check for needed packages, which uses direct dependencies
        l[['reverse']] <- FALSE
        l[['db']] <- db
        d <- do.call(tools::package_dependencies, l)
      }
      needed <- unique(unlist(d[!names(d) %in% c(pkgs, depends)]))

      toRemoveDeps <- depends[!depends %in% needed]
      if (length(toRemoveDeps)) {
        purgePackage(toRemoveDeps, db, t, repoPath)
      }
    }
  })

  if (writePACKAGES) invisible(updateRepoIndex(path = path, type = type, Rversion = Rversion))
}

# given packages, reconstruct file names and remove these files
purgePackage <- function(pkgs, db, type, repoPath) {
  w <- which(db[, "Package"] %in% pkgs)

  if (length(w)) {
    toRemove <- paste0(db[w, "Package"], "_", db[w, "Version"], pkgFileExt(type))
    file.remove(file.path(repoPath, toRemove))
  }
}
