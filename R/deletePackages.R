#' Remove packages from a miniCRAN repository.
#' @inheritParams addPackage
#' @param deps logical indicating whether the package reverse dependencies should be removed (default `TRUE`).
#' @param recursive (only if `deps = TRUE`) logical indicating whether dependencies should be removed recursively (default `TRUE`)
#' @importFrom tools package_dependencies
#' @export
#' @family update repo functions
deletePackage <- function(pkgs = NULL, path = NULL, # repos = getOption("repos"), -- needed for pkgDep
                          type = "source", Rversion = R.version,
                          writePACKAGES = TRUE, deps = TRUE, recursive = TRUE) {
    if (is.null(path) || is.null(pkgs)) stop("path and pkgs must both be specified.")

    sapply(type, function(t) {
        repoPath <- file.path(path, repoPrefix(t, Rversion))

        a <- pkgAvail(repos = path, type = t, Rversion = Rversion)
        w <- which(a[, "Package"] %in% pkgs)
        if (length(w)) {
            ext <- switch(t, source = ".tar.gz", mac.binary = ".tgz", win.binary = ".zip")
            toRemove <- paste0(a[w, "Package"], "_", a[w, "Version"], ext)
            unlink(file.path(path, repoPath, toRemove))

            if (deps) {
                d <- package_dependencies(pkgs, a, reverse = TRUE, recursive = recursive)
                depends <- if (!is.null(d[[pkgs]])) d[[pkgs]] else character()
                needed <- unique(unlist(d[!names(d) %in% c(pkgs, depends)]))
                toRemoveDeps <- depends[!depends %in% needed]
                w <- which(a[, "Package"] %in% toRemoveDeps)
                toRemoveDeps <- paste0(a[w, "Package"], "_", a[w, "Version"], ext)
                unlink(file.path(path, repoPath, toRemoveDeps))
            }
        }
    })

    if (writePACKAGES) invisible(updateRepoIndex(path = path, type = type, Rversion = Rversion))
}
