#' Retrieves package dependencies.
#' 
#' Performs recursive retrieve for \code{Depends}, \code{Imports} and \code{LinkLibrary}. Performs non-recursive retrieve for \code{Suggests}.
#' 
#' This 
#' 
#' @param pkg Character vector of packages.
#' @param availPkgs Vector of available packages.  Defaults to reading this list from CRAN, using \code{\link{available.packages}}
#' @param repos URL(s) of the 'contrib' sections of the repositories. Passed to \code{\link{available.packages}}
#' @param type Passed to \code{\link{available.packages}}
#' @param depends If TRUE, retrieves Depends, Imports and LinkingTo dependencies (non-recursively)
#' @param suggests If TRUE, retrieves Suggests dependencies (non-recursively)
#' @param ... Other arguments passed to \code{\link{available.packages}}
#' 
#' @export
#' @family miniCRAN
#' 
#' @examples
#' \dontrun{
#' pkgDep(c("ggplot2", "plyr", "reshape2"))
#' }

pkgDep <- function(pkg, availPkgs, repos=getOption("repos"), type=getOption("pkgType"), depends=TRUE, suggests=FALSE, ...){
  if(!depends & !suggests) {
    warning("Returning nothing, since depends and suggests are both FALSE")
    return(character(0))
  }
  
  if(missing(pkg) || !is.character(pkg)){
    stop("pkg should be a character vector with package names")
  }
  if(repos["CRAN"] == "@CRAN@"){
    warning("It seems that your CRAN mirror is set incorrectly")
  }
  if(is.na(type)) type <- "source"
  if(missing(availPkgs)){
    availPkgs <- available.packages(contriburl=contrib.url(repos, type=type), ...)
  }
  if(nrow(availPkgs) == 0){
    stop("Unable to retrieve available packages from CRAN")
  }
  
  pkgInAvail <- pkg %in% availPkgs[, "Package"]
  if(sum(pkgInAvail) == 0 ) stop("No valid packages in pkg")
  if(sum(pkgInAvail) < length(pkg)){
    warning("Package not recognized: ", paste(pkg[!pkgInAvail], collapse=", "))
  }
  
  pkgAvail <- pkg[pkgInAvail]
  
  
  if(depends){
    x <- tools::package_dependencies(pkgAvail, availPkgs, recursive=TRUE)
    x1 <- unique(unname(unlist(x)))
  } else {
    x1 <- character(0)
  }
  if(suggests){
    x <- tools::package_dependencies(pkgAvail, availPkgs, which="Suggests", recursive=FALSE)
    x2 <- unique(unname(unlist(x)))
  } else {
    x2 <- character(0)
  }
  sort(unique(c(pkgAvail, x1, x2)))
}

#' Reads available packages from CRAN repository.
#' 
#' This is a thin wrapper around \code{\link{available.packages}}.  If the argument \code{path} is supplied, then the function attempts to read from a local repository, otherwise attempts to read from a CRAN mirror at the \code{repos} url.
#' 
#' @param path If supplied, locates available packages from local path
#' @inheritParams pkgDep
#' @export
#' @family miniCRAN
pkgAvail <- function(path, repos=getOption("repos"), type=getOption("pkgType"), ...){
  if(!missing("path")) {
    if(!file.exists(path)) stop("path does not exist")
    contriburl <- contrib.url(paste0("file:///", path), type=type)
  } else {
    contriburl <- contrib.url(repos, type=type)
  }
  available.packages(contriburl=contriburl, type=type, ...)
}