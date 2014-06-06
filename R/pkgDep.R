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
#' @param enhances If TRUE, retrieves Enhances dependencies (non-recursively)
#' @param path Destination download path
#' @param ... Other arguments passed to \code{\link{available.packages}}
#' 
#' @export
#' @seealso makeDepGraph
#' @family miniCRAN
#' 
#' @examples
#' pkgDep(pkg=
#'   c("ggplot2", "plyr", "reshape2"), 
#'   repos=c(CRAN="http://cran.revolutionanalytics.com")
#' )

pkgDep <- function(pkg, availPkgs, repos=getOption("repos"), type=getOption("pkgType"), depends=TRUE, suggests=FALSE, enhances=FALSE, path, includeBase=FALSE, ...){
  if(!depends & !suggests & !enhances) {
    warning("Returning nothing, since depends, suggests and enhances are all FALSE")
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
    availPkgs <- pkgAvail(path=path, repos=repos, type=type, ...)
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
  if(enhances){
    x <- tools::package_dependencies(pkgAvail, availPkgs, which="Enhances", recursive=FALSE)
    x3 <- unique(unname(unlist(x)))
  } else {
    x3 <- character(0)
  }
  ret <- sort(unique(c(pkgAvail, x1, x2, x3)))
  if(!includeBase) ret <- ret[ret %in% rownames(availPkgs)]
  ret
}

#' Reads available packages from CRAN repository.
#' 
#' This is a thin wrapper around \code{\link{available.packages}}.  If the argument \code{path} is supplied, then the function attempts to read from a local repository, otherwise attempts to read from a CRAN mirror at the \code{repos} url.
#' 
#' @inheritParams pkgDep
#' @export
#' @family miniCRAN
pkgAvail <- function(repos=getOption("repos"), type=getOption("pkgType"), ...){
  if(!grepl("^file", repos) && file.exists(repos)) {
    repos <- paste0("file:///", repos)
  }
  available.packages(contrib.url(repos, type=type))
}

