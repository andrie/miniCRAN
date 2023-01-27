#' Delete packages from a miniCRAN repository.
#' @inheritParams addPackage
#' @param deps logical indicating whether the package reverse dependencies should be removed (default `TRUE`).
#' @inheritParams tools::package_dependencies which recursive
#' @export
#' @family update repo functions
deletePackage <- function(pkgs = NULL, path = NULL,
                          type = "source", Rversion = R.version,
                          writePACKAGES = TRUE, deps = TRUE,
                          which = "strong", recursive = FALSE, reverse = FALSE) {
  if (is.null(path) || is.null(pkgs)) stop("path and pkgs must both be specified.")

  sapply(type, function(t) {
    repoPath <- file.path(path, repoPrefix(t, Rversion))
    db <- pkgAvail(repos = path, type = t, Rversion = Rversion)
    purgePackage(pkgs, db, t, repoPath)

    if (deps) {
      pdb <- tools::package_dependencies(db = db, which = which,
                                         recursive = recursive, reverse = reverse)
      depends <- unique(unlist(lapply(pkgs, function(pkg) {
        if (!is.null(pdb[[pkg]])) pdb[[pkg]] else character()
      })))
      if (reverse) {
        # Check for needed packages, which uses direct dependencies
        pdb <- tools::package_dependencies(db = db, which = which,
                                           recursive = recursive, reverse = FALSE)
      }
      needed <- unique(unlist(pdb[!names(pdb) %in% c(pkgs, depends)]))

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
