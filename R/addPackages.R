#' Check for previous versions of packages in a miniCRAN repository.
#'
#' Checks for previous versions, and returns the file paths for packages with multiple versions. You can subsequently decide which version to keep.
#'
#' @param pkgs Character vector of packages to be installed. If not provided, checks all files for multiple package versions.
#'
#' @param path The local path to the directory where the miniCRAN repo resides.
#'
#' @param type  character, indicating the type of package to download and install. See \code{\link{install.packages}}.
#'
#' @param Rversion numeric version of the R system for which to fetch packages. See \code{\link{R_system_version}}.
#'
#' @return Returns invisibly the filepaths to packages with multiple versions for removal.
#'
#' @export
#' @family update repo functions
#'
#' @example /inst/examples/example_checkVersions.R
#'
checkVersions <- function(pkgs=NULL, path=NULL, type="source",
                          Rversion=R.version) {
  if (is.null(path)) stop("path must be specified.")
  if (!file.exists(path)) stop("invalid path, ", path)
  pkgPath <- repoBinPath(path, type, Rversion)
  if (is.null(pkgs)) {
    files = dir(pkgPath)
  } else {
    files = sapply(pkgs, function(x) list.files(pkgPath, pattern=paste0(x,"_")) )
  }
  files = unlist(files)
  pkgFiles = grep("\\.(tar\\.gz|zip|tgz)$", basename(files), value=TRUE)

  # identify duplicate packages and warn the user
  pkgs = sapply(strsplit(files, "_"), "[[", 1)
  dupes = pkgs[duplicated(pkgs)]
  if (length(dupes)) warning("Duplicate package(s): ", paste(dupes, collapse=", "))
  return(invisible(file.path(pkgPath, pkgFiles)))
}


#  ------------------------------------------------------------------------


#' Add packages to a miniCRAN repository.
#'
#' @inheritParams makeRepo
#' @inheritParams pkgDep
#'
#' @param Rversion numeric version of the R system for which to fetch packages. See \code{\link{R_system_version}}.
#'
#' @param deps logical indicating whether the package dependencies should be added (default \code{TRUE}).
#'
#' @return Installs the packages, rebuilds the package index, and invisibly returns the number of packages written to the index files.
#'
#' @importFrom tools write_PACKAGES
#' @export
#' @family update repo functions
#'
#' @example /inst/examples/example_checkVersions.R
#'
addPackage <- function(pkgs=NULL, path=NULL, repos=getOption("repos"),
                       type="source", Rversion=R.version,
                       writePACKAGES=TRUE, deps=TRUE, quiet=FALSE) {
  if (is.null(path) || is.null(pkgs)) stop("path and pkgs must both be specified.")

  lapply(type, function(t) {
    prev <- checkVersions(pkgs=pkgs, path=path, type=t, Rversion=Rversion)
    prev.df <- getPkgVersFromFile(prev)

    if (deps) pkgs <- pkgDep(pkgs, repos=repos, type=t)

    makeRepo(pkgs=pkgs, path=path, repos=repos, type=t, Rversion=Rversion,
             download=TRUE, writePACKAGES=FALSE, quiet=quiet)

    if (length(prev)) {
      curr <- suppressWarnings(
        checkVersions(pkgs=pkgs, path=path, type=t, Rversion=Rversion)
      )
      curr.df <- getPkgVersFromFile(curr)

      dupes <- with(curr.df, package[duplicated(package)])
      if (length(dupes)) {
        old <- lapply(dupes, function(x) { grep(paste0("^", x), basename(prev)) } )
        file.remove(prev[unlist(old)])
      }
    }
  })

  n <- if (writePACKAGES) updateRepoIndex(path=path, type=type, Rversion=Rversion)
  return(invisible(n))
}



#  ------------------------------------------------------------------------


#' Add old package versions to a miniCRAN repository.
#'
#' Will download and add older source package versions. Older binary versions are not normally available on CRAN and should be build from source on the platform for which they are required. As such, specifying \code{type!="source"} will likely fail as the download will not be successful.
#'
#' @inheritParams addPackage
#' @inheritParams makeRepo
#'
#' @param vers The package version to install.
#'
#' @return Adds the packages, rebuilds the package index, and invisibly returns the number of packages written to the index files.
#'
#' @note Dependencies for old package versions cannot be determined automatically and must be specified by the user in \code{pkgs} and \code{vers}. Thus, \code{deps=FALSE} is the default for this function.
#'
#' @importFrom tools write_PACKAGES
#' @export
#' @family update repo functions
#'
#' @example /inst/examples/example_checkVersions.R
#'
addOldPackage <- function(pkgs=NULL, path=NULL, vers=NULL,
                          repos=getOption("repos"),
                          type="source", Rversion=R.version,
                          writePACKAGES=TRUE, deps=FALSE, quiet=TRUE) {
  if (is.null(path) || is.null(pkgs) || is.null(vers)) {
    stop("path, pkgs, and vers must all be specified.")
  }
  if (type!="source") stop("Older binary versions are not normally available on CRAN. ",
                           "You must build the binary versions from source.")
  if(deps) {
    message("Unable to automatically determine dependency version information.\n",
            "Use pkgs and vers to identify which dependecies and their versions to download.")
  }
  vers <- as.character(vers)
  oldPkgs <- file.path(repos, repoPrefix(type, R.version), "Archive",
                       pkgs, sprintf("%s_%s%s", pkgs, vers, pkgFileExt(type)))

  pkgPath <- repoBinPath(path=path, type=type, Rversion=Rversion)
  if(!file.exists(pkgPath)) dir.create(pkgPath, recursive=TRUE)
  sapply(oldPkgs, function(x) {
    result <- utils::download.file(x, destfile=file.path(pkgPath, basename(x)),
                                   method="auto", mode="wb", quiet=quiet)
    if(result!=0) warning("error downloading file ", x)
  })
  if (writePACKAGES) invisible(updateRepoIndex(path=path, type=type, Rversion))
}
