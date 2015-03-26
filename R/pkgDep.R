#' Returns names of base packages.
#'
#' Retrieves names of installed packages by calling \code{\link[utils]{installed.packages}} and returning only those packages where \code{Priority} equals "base".
#'
#' @export
#' @family dependency functions
#' 
#' @seealso \code{\link{pkgDep}}
basePkgs <- function()names(which(installed.packages()[, "Priority"] == "base"))



#' Retrieves package dependencies.
#'
#' Performs recursive retrieve for \code{Depends}, \code{Imports} and \code{LinkLibrary}. Performs non-recursive retrieve for \code{Suggests}.
#'
#'
#' @param pkg Character vector of packages.
#' 
#' @param availPkgs Vector of available packages.  Defaults to reading this list from CRAN, using \code{\link{available.packages}}
#' 
#' @param repos URL(s) of the 'contrib' sections of the repositories, e.g. \code{"http://cran.us.r-project.org"}. Passed to \code{\link{available.packages}}
#' 
#' @param type Possible values are (currently) "source", "mac.binary" and "win.binary": the binary types can be listed and downloaded but not installed on other platforms.  Passed to \code{\link{download.packages}}.
#' 
#' @param depends If TRUE, retrieves Depends, Imports and LinkingTo dependencies (non-recursively)
#' @param suggests If TRUE, retrieves Suggests dependencies (non-recursively)
#' @param enhances If TRUE, retrieves Enhances dependencies (non-recursively)
#' 
#' @param includeBasePkgs If TRUE, include base R packages in results
#' @param Rversion Version of R. Can be specified as a character string with the two digit R version, e.g. "3.1".  Defaults to \code{\link{R.version}}
#' @param ... Other arguments passed to \code{\link{available.packages}}
#'
#' @export
#' @family dependency functions
#'
#' @example /inst/examples/example_pkgDep.R

pkgDep <- function(pkg, availPkgs, repos=getOption("repos"), type="source", depends=TRUE, suggests=TRUE, enhances=FALSE, includeBasePkgs=FALSE, Rversion = R.version, ...){
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
    availPkgs <- pkgAvail(repos=repos, type=type, Rversion = Rversion, ...)
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
#' @family create repo functions
#' @seealso \code{\link{pkgDep}}
pkgAvail <- function(repos=getOption("repos"), type="source", Rversion = R.version){
  if(!grepl("^http://|file:///", repos[1]) && file.exists(repos[1])) {
     repos <- paste0("file:///", normalizePath(repos[1], mustWork = FALSE, winslash = "/"))
  } else {
    if(!is.null(names(repos)) && repos["CRAN"] == "@CRAN@"){
      repos <- c(CRAN="http://cran.revolutionanalytics.com")
    }
  }
  utils::available.packages(contribUrl(repos, type=type, Rversion = Rversion), type=type, filters=list())
}


# Modified copy of utils::contrib.url()
contribUrl <- function (repos, type = getOption("pkgType"), Rversion = R.version) {
  Rversion <- twodigitRversion(Rversion)
  if (type == "both") 
    type <- "source"
  if (type == "binary") 
    type <- .Platform$pkgType
  if (is.null(repos)) 
    return(NULL)
  if ("@CRAN@" %in% repos && interactive()) {
    cat(gettext("--- Please select a CRAN mirror for use in this session ---"), 
        "\n", sep = "")
    flush.console()
    chooseCRANmirror()
    m <- match("@CRAN@", repos)
    nm <- names(repos)
    repos[m] <- getOption("repos")["CRAN"]
    if (is.null(nm)) 
      nm <- rep("", length(repos))
    nm[m] <- "CRAN"
    names(repos) <- nm
  }
  if ("@CRAN@" %in% repos) 
    stop("trying to use CRAN without setting a mirror")
  ver <- Rversion
  mac.path <- "macosx"
  if (substr(type, 1L, 11L) == "mac.binary.") {
    mac.path <- paste(mac.path, substring(type, 12L), sep = "/")
    type <- "mac.binary"
  }
  res <- switch(type, 
                source = paste(gsub("/$", "", repos), "src", "contrib", sep = "/"), 
                mac.binary = paste(gsub("/$", "", repos), "bin", mac.path, "contrib", ver, sep = "/"), 
                win.binary = paste(gsub("/$", "", repos), "bin", "windows", "contrib", ver, sep = "/"))
  res
}
