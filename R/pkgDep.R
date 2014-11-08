#' Returns names of base packages.
#'
#' Retrieves names of installed packages by calling \code{\link[utils]{installed.packages}} and returning only those packages where \code{Priority} equals "base".
#'
#' @export
#' @family miniCRAN functions
#' @seealso \code{\link{pkgDep}}
basePkgs <- function()names(which(installed.packages()[, "Priority"] == "base"))



#' Retrieves package dependencies.
#'
#' Performs recursive retrieve for \code{Depends}, \code{Imports} and \code{LinkLibrary}. Performs non-recursive retrieve for \code{Suggests}.
#'
#' This
#'
#' @param pkg Character vector of packages.
#' @param availPkgs Vector of available packages.  Defaults to reading this list from CRAN, using \code{\link{available.packages}}
#' @param repos URL(s) of the 'contrib' sections of the repositories. Passed to \code{\link{available.packages}}
#' @param type Possible values are (currently) "source", "mac.binary" and "win.binary": the binary types can be listed and downloaded but not installed on other platforms.  Passed to \code{\link{download.packages}}.
#' @param depends If TRUE, retrieves Depends, Imports and LinkingTo dependencies (non-recursively)
#' @param suggests If TRUE, retrieves Suggests dependencies (non-recursively)
#' @param enhances If TRUE, retrieves Enhances dependencies (non-recursively)
#' @param includeBasePkgs If TRUE, include base R packages in results
#' @param ... Other arguments passed to \code{\link{available.packages}}
#'
#' @export
#' @seealso \code{\link{makeDepGraph}}
#' @family miniCRAN functions
#'
#' @example /inst/examples/example_pkgDep.R

pkgDep <- function(pkg, availPkgs, repos=getOption("repos"), type="source", depends=TRUE, suggests=TRUE, enhances=FALSE, includeBasePkgs=FALSE, ...){
  if(!depends & !suggests & !enhances) {
    warning("Returning nothing, since depends, suggests and enhances are all FALSE")
    return(character(0))
  }

  if(missing(pkg) || !is.character(pkg)){
    stop("pkg should be a character vector with package names")
  }
  if(missing(availPkgs)){
    if(!is.null(names(repos)) & repos["CRAN"] == "@CRAN@"){
      repos <- c(CRAN="http://cran.revolutionanalytics.com")
    }
    if(is.na(type)) type <- "source"
    availPkgs <- pkgAvail(repos=repos, type=type, ...)
  }
  if(nrow(availPkgs) == 0){
    stop("Unable to retrieve available packages from CRAN")
  }

  pkgInAvail <- pkg %in% availPkgs[, "Package"]
  if(sum(pkgInAvail) == 0 ) stop("No valid packages in pkg")
  if(sum(pkgInAvail) < length(pkg)){
    warning("Package not recognized: ", paste(pkg[!pkgInAvail], collapse=", "))
  }

  n_req <- pkg[pkgInAvail]
  n_req_all <- pkg

  # Suggests
  if(suggests) {
    p_sug <- tools::package_dependencies(n_req, availPkgs,
                                         which="Suggests", recursive=FALSE)
    n_sug <- unique(unname(unlist(p_sug)))
    n_req_all <- c(n_req_all, n_sug)
  } else{
    p_sug <- NA
  }

  # Enhances
  if(enhances){
    p_enh <- tools::package_dependencies(n_req, availPkgs,
                                         which="Enhances", recursive=FALSE)
    n_enh <- unique(unname(unlist(p_enh)))
    n_req_all <- c(n_req_all, n_enh)
  } else {
    p_enh <- NA
  }

  # Depends, Imports and LinkingTo
  p_dep <- tools::package_dependencies(n_req_all, availPkgs,
                                       which=c("Depends", "Imports", "LinkingTo"),
                                       recursive=TRUE)
  n_dep <- unique(unname(unlist(p_dep)))



  p_all <- p_dep
  n_all <- unique(c(n_dep, n_req_all))
  n_all <- c(n_req, setdiff(n_all, n_req))

  ret <- n_all
  if(!includeBasePkgs) ret <- ret[!ret %in% basePkgs()]
  attr(ret, "pkgs") <- list(
    n_req = n_req,
    n_all = n_all,
    p_dep = p_dep,
    p_sug = p_sug,
    p_enh = p_enh,
    p_all = p_all
    )
  class(ret) <- c("pkgDep", "character")
  ret
}

#' @export
print.pkgDep <- function(x, ...){
  attr(x, "pkgs") <- NULL
  class(x) <- "character"
  print(as.vector(x), ...)
}

#' Reads available packages from CRAN repository.
#'
#' This is a thin wrapper around \code{\link[utils]{available.packages}}.  If the argument \code{path} is supplied, then the function attempts to read from a local repository, otherwise attempts to read from a CRAN mirror at the \code{repos} url.
#'
#' @inheritParams pkgDep
#' @export
#' @family miniCRAN functions
#' @seealso \code{\link{pkgDep}}
pkgAvail <- function(repos=getOption("repos"), type="source", ...){
  if(!grepl("^file", repos[1]) && file.exists(repos[1])) {
    repos <- paste0("file:///", repos[1])
  } else {
    if(!is.null(names(repos)) && repos["CRAN"] == "@CRAN@"){
      repos <- c(CRAN="http://cran.revolutionanalytics.com")
    }
  }
  available.packages(contrib.url(repos, type=type), type=type, filters=list())
}

