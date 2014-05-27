#' Downloads packages from CRAN to specified path.
#' 
#' Given a list of packages, downloads to a specified destination folder, then creates PACKAGES file.
#' 
#' Uses \code{\link{download.packages}} and \code{\link[tools]{write_PACKAGES}}
#' 
#' @inheritParams pkgDep
#' @param pkgs Character vector of packages to download
#' @param download If TRUE downloads packages, otherwise just creates PACKAGES file
#' @param type Passed to \code{\link{download.packages}}
#' 
#' @export
#' @family miniCRAN
makeRepo <- function(pkgs, path, download=FALSE, type="source"){
  if(!file.exists(path)) stop("Download path does not exist")
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(normalizePath(path))
  message(getwd())
  if(download) download.packages(pkgs, destdir=path, type=type)
  tools::write_PACKAGES(dir=".", type=type) 
}

makeLibrary <- function(pkgs, path, download=FALSE, type="source"){
  
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
