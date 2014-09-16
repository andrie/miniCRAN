#' Downloads packages from CRAN to specified path and creates a local repository.
#' 
#' Given a list of packages, downloads to a specified destination folder using the required CRAN folder structure then creates the PACKAGES index file.
#' 
#' The function \code{makeRepo} creates a repository, similar in structure to CRAN.  It optionally updates the PACKAGES file.  If done correctly, it is possible to use this folder as a repository, i.e. it will support functions like \code{\link[utils]{install.packages}()}.
#' 
#' 
#' Uses \code{\link{download.packages}} and \code{\link[tools]{write_PACKAGES}}
#' 
#' @inheritParams pkgDep
#' @param pkgs Character vector of packages to download
#' @param path Destination download path
#' @param type Passed to \code{\link{download.packages}}
#' @param Rversion String of format "<major R version>.<minor R version>", e.g. "3.2". Only used if \code{type} is not "source"
#' @param download If TRUE downloads packages, otherwise just creates PACKAGES file
#' @param writePACKAGES If TRUE, calls \code{\link[tools]{write_PACKAGES}} to update the repository PACKAGES file
#' 
#' @export
#' @example \inst\examples\example_makeRepo.R

makeRepo <- function(pkgs, path, repos=getOption("repos"), type="source", 
                     Rversion=R.version, download=TRUE, writePACKAGES=TRUE){
  if(!file.exists(path)) stop("Download path does not exist")
  Rversion <- twodigitRversion(Rversion)
  
  folder <- switch(
    type,
    "source" = "src/contrib",
    "win.binary" = sprintf("bin/windows/contrib/%s", Rversion),
    "mac.binary" = sprintf("bin/macosx/contrib/%s", Rversion),
    "mac.binary.mavericks" =  sprintf("bin/macosx/mavericks/contrib/%s", Rversion),
    "mac.binary.leopard"= sprintf("bin/macosx/leopard/contrib/%s", Rversion),
    stop("Type ", type, "not recognised.")
  )
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




twodigitRversion <- function(R = R.version){
  paste(R$major, strsplit(R$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
}




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

