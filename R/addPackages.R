
readDescription <- function (file) {
  trimSpaces <- function(x){
    gsub("^\\s+|\\s+$", "", x)
  }

  prepDescription <- function(d){
    clean <- function(x)gsub("\n", " ", x)
    if(is.null(d$Depends)) d$Depends <- NA else d$Depends <- clean(d$Depends)
    if(is.null(d$Imports)) d$Imports <- NA else d$Imports <- clean(d$Imports)
    if(is.null(d$Suggests)) d$Suggests <- NA else d$Suggests <- clean(d$Suggests)
    if(is.null(d$LinkingTo)) d$LinkingTo <- NA else d$LinkingTo <- clean(d$LinkingTo)
    if(is.null(d$Enhances)) d$Enhances <- NA else d$Enhances <- clean(d$Enhances)
    d
  }
  dcf <- read.dcf(file)
  dcfList <- setNames(as.list(dcf[1, ]), colnames(dcf))
  ret <- lapply(dcfList, trimSpaces)
  prepDescription(ret)
}




# from http://stackoverflow.com/questions/13163248/possible-to-override-the-blocking-of-a-packages-re-installation-after-it-has


#' @importFrom httr GET stop_for_status content
readDescriptionGithub <- function(repo, username, branch="master", quiet=TRUE){
  if(!missing(username) && !is.null(username)) repo <- paste(username, repo, sep="/")
  pkg <- sprintf("https://github.com/%s/raw/%s/DESCRIPTION", repo, branch)
  ff <- tempfile()
  on.exit(unlink(ff))
#   download.file(url=pkg, destfile=ff, quiet=quiet, method="curl")
  request <- GET(pkg)
  stop_for_status(request)
  writeBin(content(request), ff)

  readDescription(ff)
}



addPackage <- function(pdb, dcf, warnings=TRUE){
  pkgName <- dcf[["Package"]]
#   pkgRow <- match(pkgName, rownames(pdb))
  pkgRow <- match(pkgName, pdb[, "Package"])
  newRow <- with(dcf,
                 c(Package, Version, NA, Depends, Imports, LinkingTo, Suggests, Enhances, License,
                   rep(NA, 8)))
  if(!is.na(pkgRow)){
    pdb[pkgRow, ] <- newRow
    if(warnings) {
      msg <- sprintf("Overwriting package information for: %s", pkgName)
      warning (msg)
    }
  } else {
    pdb <- rbind(pdb, newRow)
    rownames(pdb)[nrow(pdb)] <- pkgName
  }
  pdb
}

#' Add DESCRIPTION information from package on github.
#'
#' Downloads the DESCRIPTION file from a package on github, parses the fields and adds (or replaces) a row in the available package database.
#'
#' @param pdb Package database, usually the result of \code{\link{pkgAvail}} or \code{\link{available.packages}}
#' @param repo Character vector. Name of repository on github, e.g. \code{"RevolutionAnalytics/checkpoint"}
#' @param username Optional character vector. Name of repository on github, e.g. \code{"RevolutionAnalytics/checkpoint"}
#' @param branch name of branch, defaults to \code{"master"}
#' @export
#' @example \inst\examples\example_addPackage.R
addPackageGithub <- function(pdb=pkgAvail(), repo, username=NULL, branch="master"){
  desc <- readDescriptionGithub(repo=repo, username=username, branch=branch)
  addPackage(pdb, desc)
}

################################################################################
#' Check for previous versions of packages in a miniCRAN repository.
#'
#' Checks for previous versions, and returns the file paths for packages with
#' multiple versions. The admin can subsequently decide which version to keep.
#'
#' @param pkgs Character vector of packages to be installed. If not provided,
#'              checks all files for multiple package versions.
#'
#' @param path  The local path to the directory where the miniCRAN repo resides.
#'
#' @param type  character, indicating the type of package to download and
#'  install. See \code{\link{install.packages}}.
#'
#' @param Rversion numeric version of the R system for which to fetch packages.
#' See \code{\link{R_system_version}}.
#'
#' @return Returns filepaths to packages with multiple versions for removal.
#'
#' @export
#' @docType methods
#'
#' @example \inst\examples\example_checkVersions.R
#'
checkVersions <- function(pkgs=NULL, path=NULL, type="source",
                          Rversion=getRversion()) {
  if (is.null(path)) stop("path must be specified.")
  if (!file.exists(path)) stop("invalid path, ", path)
  pkgPath <- file.path(path, repoPrefix(type, Rversion))
  if (is.null(pkgs)) {
    files = dir(pkgPath)
  } else {
    files = sapply(pkgs, function(x) list.files(pkgPath, pattern=paste0(x,"_")) )
  }
  files = unlist(files)

  # identify duplicate packages and warn the user
  pkgs = sapply(strsplit(files, "_"), "[[", 1)
  dupes = pkgs[duplicated(pkgs)]
  if (length(dupes)) warning("Duplicate package(s):", dupes)
  return(file.path(pkgPath, files))
}

################################################################################
#' Add packages to a miniCRAN repository.
#'
#' @param pkgs Character vector of packages to be installed.
#'
#' @param path  The local path to the directory where the miniCRAN repo resides.
#'
#' @param repos character vector, the base URL(s) of the repositories to use,
#' e.g., the URL of a CRAN mirror such as "\code{http://cran.us.r-project.org}".
#'
#' @param type  character, indicating the type of package to download and
#'  install. See \code{\link{install.packages}}.
#'
#' @param Rversion numeric version of the R system for which to fetch packages.
#' See \code{\link{R_system_version}}.
#'
#' @param writePACKAGES If TRUE, calls \code{\link[tools]{write_PACKAGES}} to
#' update the repository PACKAGES file.
#'
#' @param deps logical indicating whether the package dependencies should be
#' added (default \code{TRUE}).
#'
#' @return Installs the packages, rebuilds the package index invisibly returns
#' the number of packages writen to the index files.
#'
#' @importFrom tools write_PACKAGES
#' @export
#' @docType methods
#'
#' @example \inst\examples\example_checkVersions.R
#'
add.packages.miniCRAN <- function(pkgs=NULL, path=NULL, repos=getOption("repos"),
                                  type="source", Rversion=R.version,
                                  writePACKAGES=TRUE, deps=TRUE) {
  if (is.null(path) || is.null(pkgs)) stop("path and pkgs must both be specified.")
  prev <- checkVersions(pkgs=pkgs, path=path, type=type, Rversion=Rversion)
  if (deps) pkgs <- pkgDep(pkgs)
  makeRepo(pkgs=pkgs, path=path, repos=repos, type=type, Rversion=Rversion,
           download=TRUE, writePACKAGES=FALSE)
  if (length(prev)>0) {
    curr <- checkVersions(pkgs=pkgs, path=path, type=type, Rversion=Rversion)
    old <- setdiff(prev, curr)
    message("Removing previous versions of newly added packages:")
    message(basename(old))
    file.remove(old)
  }
  if (writePACKAGES) {
    pkgPath <- file.path(path, repoPrefix(type, Rversion))
    tools::write_PACKAGES(dir=pkgPath, type=type)
  }
}

################################################################################
#' Add old package versions to a miniCRAN repository.
#'
#' Will download and add older source package versions. Older binary versions
#' are not normally available on CRAN and should be build from source on the
#' platform for which they are required. As such, specifying \code{type!="source"}
#' will likely fail as the download will not be successful.
#'
#' @inheritParams add.packages.miniCRAN
#'
#' @param vers The package version to install.
#'
#' @return Installs the packages, rebuilds the package index invisibly returns
#' the number of packages writen to the index files.
#'
#' @note Dependencies for old package versions cannot be determined automatically
#' and must be specified by the user in `pkgs` and `vers`. Thus, \code{deps=FALSE}
#' is the default for this function.
#'
#' @importFrom tools write_PACKAGES
#' @export
#' @docType methods
#'
#' @example \inst\examples\example_checkVersions.R
#'
addOldPackage <- function(pkgs=NULL, path=NULL, vers=NULL,
                          repos=getOption("repos"),
                          type="source", Rversion=R.version,
                          writePACKAGES=TRUE, deps=FALSE) {
  if (is.null(path) || is.null(pkgs) || is.null(vers)) {
    stop("path, pkgs, and vers must all be specified.")
  }
  if (type!="source") warning("Older binary versions are not normally available on CRAN. ",
                              "You must build the binary versions from source.")
  if(deps) {
    message("Unable to automatically determine dependency version information.")
    message("Use `pkgs` and `vers` to identify which dependecies and there versions to download.")
  }
  vers <- as.character(vers)
  pkgFileExt <- function(type) {
    switch(
      type,
      "source" = ".tar.gz",
      "win.binary" = ".zip",
      "mac.binary" = ".tgz",
      "mac.binary.mavericks" = ".tgz",
      "mac.binary.leopard"= ".tgz",
      stop("Type ", type, "not recognised.")
    )
  }
  oldPkgs <- file.path(repos, repoPrefix(type, R.version), "Archive",
                       pkgs, sprintf("%s_%s%s", pkgs, vers, pkgFileExt(type)))

  pkgPath <- file.path(path=path, repoPrefix(type, R.version))
  dir.create(pkgPath, recursive=TRUE)
  sapply(oldPkgs, function(x) {
    download.file(x, destfile=file.path(pkgPath, basename(x)))
  })
  if (writePACKAGES) tools::write_PACKAGES(path=pkgPath, type=type)
}
