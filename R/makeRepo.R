#' Downloads packages from CRAN to specified path and creates a local repository.
#'
#' Given a list of packages, downloads these packages to a specified destination
#' folder using the required CRAN folder structure, and finally creates the
#' PACKAGES index file.  Since the folder structure mimics the required
#' structure and files of a CRAN repository, it supports functions like
#' \code{\link[utils]{install.packages}()}.
#'
#' @section Repo folder structure:
#' The folder structure of a repository
#' \itemize{
#'  \item{Root}
#'  \itemize{
#'    \item{src}
#'    \itemize{
#'      \item{contrib}
#'    }
#'  \item{bin}
#'  \itemize{
#'    \item{windows/contrib/}
#'    \item{macosx/contrib/}
#'    \item{macosx/mavericks/contrib}
#'    \item{macosx/leopard/contrib}
#'  }
#'  \item{PACKAGES}
#'  }
#' }
#'
#' @note Internally makes use of \code{\link[utils]{download.packages}} and
#' \code{\link[tools]{write_PACKAGES}}
#'
#' @inheritParams pkgDep
#' @param pkgs Character vector of packages to download
#' @param path Destination download path. This path is the root folder of your
#' new repository.
#' @param Rversion List with two named elements: `major` and `minor`.
#' If not supplied, defaults to system version of R, using
#' \code{\link[base]{R.version}}.  Only used if \code{type} is not "source"
#' @param download If TRUE downloads packages.
#' @param writePACKAGES If TRUE, calls \code{\link[tools]{write_PACKAGES}} to
#' update the repository PACKAGES file.
#'
#' @export
#' @family miniCRAN functions
#' @example \inst\examples\example_makeRepo.R
makeRepo <- function(pkgs, path, repos=getOption("repos"), type="source",
                     Rversion=R.version, download=TRUE, writePACKAGES=TRUE) {
  if(!file.exists(path)) stop("Download path does not exist")
  folder <- repoPrefix(type, Rversion)
  pkgPath <- file.path(path, folder)
  if(!file.exists(pkgPath)) {
    result <- dir.create(pkgPath, recursive=TRUE)
    if(result) {
      message("Created new folder: ", pkgPath)
    } else {
      stop("Unable to create repo path: ", pkgPath)
    }
  }

  if(download) download.packages(pkgs, destdir=pkgPath, repos=repos, type=type)
  if(writePACKAGES) tools::write_PACKAGES(dir=pkgPath, type=type)
}



#' Get the path to the repo directory containing the package files.
#'
#' @section Repo folder structure:
#' The folder structure of a repository
#' \itemize{
#'  \item{Root}
#'  \itemize{
#'    \item{src}
#'    \itemize{
#'      \item{contrib}
#'    }
#'  \item{bin}
#'  \itemize{
#'    \item{windows/contrib/}
#'    \item{macosx/contrib/}
#'    \item{macosx/mavericks/contrib}
#'    \item{macosx/leopard/contrib}
#'  }
#'  \item{PACKAGES}
#'  }
#' }
#'
#' @return The filepath to the package files directory.
#' @export
repoPrefix <- function(type, Rversion){
  Rversion = twodigitRversion(Rversion)
  switch(
    type,
    "source" = "src/contrib",
    "win.binary" = sprintf("bin/windows/contrib/%s", Rversion),
    "mac.binary" = sprintf("bin/macosx/contrib/%s", Rversion),
    "mac.binary.mavericks" =  sprintf("bin/macosx/mavericks/contrib/%s", Rversion),
    "mac.binary.leopard"= sprintf("bin/macosx/leopard/contrib/%s", Rversion),
    stop("Type ", type, "not recognised.")
  )
}



#' Get a two-digit version of the R version
#'
#' @param R Either a list of the format \code{\link{R.version}}, a character
#' string (e.g., \code{"3.1.2"}), or a numeric version of the type
#' \code{\link{R_system_version}}.
#'
#' @return A character string representing the two-digit R version.
#'
#' @export
twodigitRversion <- function(R=R.version){
  if("simple.list" %in% is(R)) {
    paste(R$major, strsplit(R$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
  } else if ("R_system_version" %in% is(R)) {
    paste(strsplit(as.character(R), ".", fixed=TRUE)[[1L]][1L:2L], collapse=".")
  } else if (is.character(R)) {
    paste(strsplit(R, ".", fixed=TRUE)[[1L]][1L:2L], collapse=".")
  }
}



#' Deprecated function to download packages to local folder.
#'
#' @inheritParams makeRepo
#' @export
makeLibrary <- function(pkgs, path, type="source"){
  .Deprecated("makeRepo")
  NULL
#   if(!file.exists(path)) stop("Download path does not exist")
#   wd <- getwd()
#   on.exit(setwd(wd))
#   setwd(normalizePath(path))
#   message(getwd())
#   download.packages(pkgs, destdir=path, type=type)
}

