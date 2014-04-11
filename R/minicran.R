#' Retrieves package dependencies.
#' 
#' Performs recursive retrieve for \code{Depends}, \code{Imports} and \code{LinkLibrary}. Performs non-recursive retrieve for \code{Suggests}.
#' 
#' @param pkg Character vector of packages.
#' @param availPkgs Vector of available packages.  Defaults to reading this list from CRAN, using \code{\link{available.packages}}
#' @param contriburl URL(s) of the 'contrib' sections of the repositories. Passed to \code{\link{available.packages}}
#' @param type Passed to \code{\link{available.packages}}
#' @param ... Other arguments passed to \code{\link{available.packages}}
#' 
#' @export
#' @family Package dependency
#' 
#' @examples
#' \dontrun{
#' pkgDep(c("ggplot2", "plyr", "reshape2"))
#' }

pkgDep <- function(pkg, availPkgs, contriburl=getOption("repos"), type=getOption("pkgType"), ...){
  if(missing(pkg) || !is.character(pkg)) stop("pkg should be a character vector with package names")
  if(contriburl["CRAN"] == "@CRAN@") warning("It seems that your CRAN mirror is set incorrectly")
  if(is.na(type)) type <- "source"
  if(missing(availPkgs)) availPkgs = available.packages(contriburl=contriburl, type=type, ...)
  if(nrow(availPkgs) == 0) stop("Unable to retrieve available packages from CRAN")
  
  pkgInAvail <- pkg %in% availPkgs[, "Package"]
  if(sum(pkgInAvail) == 0 ) stop("No valid packages in pkg")
  if(sum(pkgInAvail) < length(pkg)) warning("Package not recognized: ", paste(pkg[!pkgInAvail], collapse=", "))
  
  x <- tools::package_dependencies(pkg, availPkgs, recursive=TRUE)
  x1 <- unique(unname(unlist(x)))
  x <- tools::package_dependencies(pkg, availPkgs, which="Suggests", recursive=FALSE)
  x2 <- unique(unname(unlist(x)))
  sort(unique(c(x1, x2)))
}


#  ------------------------------------------------------------------------


#' Downloads packages from CRAN to specified path.
#' 
#' Given a list of packages, downloads to a specified destination folder, then creates PACKAGES file.
#' 
#' Uses \code{\link{download.packages}} and \code{\link[tools]{write_PACKAGES}}
#' 
#' @param pkg Character vector of packages to download
#' @param path Destination download path
#' @param download If TRUE downloads packages, otherwise just creates PACKAGES file
#' @param type Passed to \code{\link{download.packages}}
#' 
#' @export
#' @family Package dependency
makeRepo <- function(pkg, path, download=FALSE, type="source"){
  if(!file.exists(path)) stop("Download path does not exist")
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(path)
  if(download) download.packages(pkg, destdir=path, type=type)
  tools::write_PACKAGES() 
}


