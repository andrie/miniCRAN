
#' Retrieves package dependencies.
#' 
#' Performs recursive retrieve for Depends, Imports and LinkLibrary. Performs non-recursive for Suggests.
#' 
#' @param pkg Character vector of packages.
#' @param availPkgs Vector of available packages.  Defaults to reading this list from CRAN, using \code{\link{available.packages}}
#' 
#' @export
#' @family Package dependency
#' 
#' @examples
#' pkgDep(c("ggplot2", "plyr", "reshape2"))

pkgDep <- function(pkg, availPkgs = available.packages()){
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


