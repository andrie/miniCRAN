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
#' @param download If TRUE downloads packages, otherwise just creates PACKAGES file
#' @param writePACKAGES If TRUE, calls \code{\link[tools]{write_PACKAGES}} to update the repository PACKAGES file
#' 
#' @export
#' @family miniCRAN
makeRepo <- function(pkgs, path, type="source", download=FALSE, writePACKAGES=TRUE){
  if(!file.exists(path)) stop("Download path does not exist")
  if(type != "source") stop("At this time, only type=\"source\" is supported")
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(normalizePath(path))
  
  folder <- switch(type,
                   "source" = "src/contrib",
                   "win.binary" = "bin/windows/contrib/x.y",
                   "mac.binary" = "bin/macosx/contrib/3.y",
                   "mac.binary.mavericks" =  "bin/macosx/mavericks/contrib/3.y",
                   "mac.binary.leopard"= "bin/macosx/leopard/contrib/2.y"
  )
  pkgPath <- file.path(path, folder)
  if(!file.exists(pkgPath)) {
    result <- dir.create(pkgPath, recursive=TRUE)
    if(result) message("Creating new folders: ", pkgPath) else stop("Unable to create repo path: ", pkgPath)

  }
  
  if(download) download.packages(pkgs, destdir=pkgPath, type=type)
  if(writePACKAGES) tools::write_PACKAGES(dir=pkgPath, type=type) 
}

#' @rdname makeRepo
#' @export
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
