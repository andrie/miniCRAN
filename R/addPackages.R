
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
#' @param path  The local path to the directory where the miniCRAN repo resides.
#'
#' @param pkgs Character vector of packages to be installed. If not provided,
#'              checks all files for multiple package versions.
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
#' @rdname add-packages-miniCRAN
#' @docType methods
#'
#' @examples
#' \dontrun{
#'  check.package.versions("/var/www/miniCRAN", "raster")
#' }
#'
check.package.versions <- function(path=NULL, pkgs=NULL, type="source",
                                   Rversion=getRversion()) {
  if (is.null(path)) stop("path must be specified.")
  if (!file.exists(path)) stop("invalid path, ", path)
  pkgPath <- file.path(path, repoPrefix(type, twodigitRversion(Rversion)))
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
#' @params deps logical indicating whether the package dependencies should be
#' added (default \code{TRUE}).
#' 
#' @return Installs the packages, rebuilds the package index and returns it.
#'
#' @import tools
#' @export
#' @rdname add-packages-miniCRAN
#' @docType methods
#'
#' @examples
#' \dontrun{
#'  pth <- "/var/www/miniCRAN"
#'  add.packages.miniCRAN(pth, c("ggplot2", "lme4"))
#'  add.packages.miniCRAN(pth, c("ggplot2", "lme4"), type="win.binary")
#' }
#'
add.packages.miniCRAN <- function(path=NULL, pkgs=NULL, repos=getOption("repos"),
                                  type="source", Rversion=R.version,
                                  writePACKAGES=TRUE, deps=TRUE) {
  if (is.null(path) || is.null(pkgs)) stop("path and pkgs must both be specified.")
  prev <- check.package.versions(path=path, pkgs=pkgs, type=type,
                                 Rversion=Rversion)
  if (deps) pkgs <- pkgDep(pkgs)
  makeRepo(pkgs=pkgs, path=path, repos=repos, type=type, Rversion=Rversion,
           download=TRUE, writePACKAGES=FALSE)
  if (length(prev)>0) {
    curr <- check.package.versions(path=path, pkgs=pkgs, type=type,
                                   Rversion=Rversion)
    old <- intersect(curr, prev)
    message("Removing previous versions of newly added packages:")
    message(basename(old))
    file.remove(old)
  }
  if (writePACKAGES) {
    pkgPath <- file.path(path, repoPrefix(type, twodigitRversion(Rversion)))
    tools::write_PACKAGES(dir=pkgPath, type=type)
  }
}
