#' Downloads packages from CRAN to specified path and creates repository or library.
#' 
#' Given a list of packages, downloads to a specified destination folder, then creates PACKAGES file.
#' 
#' The function \code{makeRepo} creates a repository, similar in structure to CRAN.  It optionally updates the PACKAGES file.  If done correctly, it is possible to use this folder as a repository, i.e. it will support functions like \code{install.packages}.
#' 
#' The function \code{makeLibrary} downloads the packages into a single folder, i.e. similar to a library on a machine.
#' 
#' Uses \code{\link{download.packages}} and \code{\link[tools]{write_PACKAGES}}
#' 
#' @inheritParams pkgDep
#' @param pkgs Character vector of packages to download
#' @param type Passed to \code{\link{download.packages}}
#' @param Rversion String of format "<major R version>.<minor R version>", e.g. "3.2". Only used if \code{type} is not "source"
#' @param download If TRUE downloads packages, otherwise just creates PACKAGES file
#' @param writePACKAGES If TRUE, calls \code{\link[tools]{write_PACKAGES}} to update the repository PACKAGES file
#' 
#' # Make repo for source and win.binary
#' makeRepo(pkgList, path=pth, repos=revolution, download=TRUE, writePACKAGES=TRUE, type="source")
#' makeRepo(pkgList, path=pth, repos=revolution, download=TRUE, writePACKAGES=TRUE, type="win.binary")
#' 
#' # List all files in miniCRAN
#' list.files(pth, recursive = TRUE)
#' 
#' # Check for available packages
#' pkgAvail(repos=pth, type="source")
#' pkgAvail(repos=pth, type="win.binary")
#' 
#' # Delete temporary folder
#' unlink(pth, recursive = TRUE)

makeRepo <- function(pkgs, path, repos=getOption("repos"), type="source", Rversion=getRversion(), download=FALSE, writePACKAGES=TRUE){
  if(!file.exists(path)) stop("Download path does not exist")
#   wd <- getwd()
#   on.exit(setwd(wd))
  
  folder <- switch(type,
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
#     setwd(pkgPath)
    if(result) message("Created new folder: ", pkgPath) else stop("Unable to create repo path: ", pkgPath)
  }

  if(download) download.packages(pkgs, destdir=pkgPath, repos=repos, type=type)
  if(writePACKAGES) tools::write_PACKAGES(dir=pkgPath, type=type) 
}




#' @rdname makeRepo
#' @export
getRversion <- function(){
  R <- R.version
  paste(R$major, strsplit(R$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
}




#' @rdname makeRepo
#' @export
#' @family makeRepo
makeLibrary <- function(pkgs, path, type="source"){
  if(!file.exists(path)) stop("Download path does not exist")
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(normalizePath(path))
  message(getwd())
  download.packages(pkgs, destdir=path, type=type)
}

# downloadPackages <- function(pkg, path, type="source"){
#   folder <- switch(type,
#                    "source" = "src/contrib",
#                    "win.binary" = "bin/windows/contrib/x.y",
#                    "mac.binary" = "bin/macosx/contrib/3.y",
#                    "mac.binary.mavericks" =  "bin/macosx/mavericks/contrib/3.y",
#                    "mac.binary.leopard"= "bin/macosx/leopard/contrib/2.y"
#   )
#   pkgPath <- file.path(pkg, folder)
#                    
# }
# 
# updatePackageIndex <- function(path, type="source"){
#   
# }
