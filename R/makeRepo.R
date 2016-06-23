#' Downloads packages from CRAN to specified path and creates a local repository.
#'
#' Given a list of packages, downloads these packages to a specified destination folder using the required CRAN folder structure, and finally creates the PACKAGES index file.  Since the folder structure mimics the required structure and files of a CRAN repository, it supports functions like \code{\link[utils]{install.packages}()}.
#'
#' @section Repo folder structure:
#' The folder structure of a repository
#' \itemize{
#'  \item{Root}
#'  \itemize{
#'    \item{src/contrib}
#'    \itemize{
#'      \item{PACKAGES}
#'    }
#'    \item{bin}
#'    \itemize{
#'      \item{windows/contrib/version}
#'      \itemize{
#'        \item{PACKAGES}
#'      }
#'      \item{macosx/contrib/version}
#'      \itemize{
#'        \item{PACKAGES}
#'      }
#'      \item{macosx/mavericks/contrib/version}
#'      \itemize{
#'        \item{PACKAGES}
#'      }
#'      \item{macosx/leopard/contrib/version}
#'      \itemize{
#'        \item{PACKAGES}
#'      }
#'    }
#'  }
#' }
#'
#' @note Internally makes use of \code{\link[utils]{download.packages}} and \code{\link{write_PACKAGES}}
#'
#' @inheritParams pkgDep
#'
#' @param pkgs Character vector of packages to download
#'
#' @param path Destination download path. This path is the root folder of your new repository.
#'
#' @param Rversion List with two named elements: `major` and `minor`. If not supplied, defaults to system version of R, using \code{\link{R.version}}.  Only used if \code{type} is not "source"
#'
#' @param download If TRUE downloads packages.
#'
#' @param quiet If TRUE, suppress status messages (if any), and the progress bar during download.
#'
#' @param writePACKAGES If TRUE, calls \code{\link{write_PACKAGES}} to update the repository PACKAGES file.
#'
#' @export
#' @family update repo functions
#'
#' @example /inst/examples/example_makeRepo.R
makeRepo <- function(pkgs, path, repos=getOption("repos"), type="source",
                     Rversion=R.version, download=TRUE, writePACKAGES=TRUE, quiet=FALSE) {
  if(!file.exists(path)) stop("Download path does not exist")

  downloaded <- lapply(type, function(type) {
    pkgPath <- repoBinPath(path=path, type=type, Rversion=Rversion)
    if(!file.exists(pkgPath)) {
      result <- dir.create(pkgPath, recursive=TRUE, showWarnings = FALSE)
      if(result) {
        if(!quiet) message("Created new folder: ", pkgPath)
      } else {
        stop("Unable to create repo path: ", pkgPath)
      }
    }

    pdb <- pkgAvail(repos = repos, type=type, Rversion = Rversion)

    if(download) {
      utils::download.packages(pkgs, destdir=pkgPath, available=pdb, repos=repos,
                               contriburl = contribUrl(repos, type, Rversion),
                               type=type, quiet=quiet)
    }
  })
  
  

  if(writePACKAGES) updateRepoIndex(path=path, type=type, Rversion=Rversion)
  if(download) sapply(downloaded, "[[", 2) else character(0)
}




#' @rdname makeRepo
#' @export
updateRepoIndex <- function(path, type="source", Rversion=R.version) {
  n <- lapply(type, function(type){
    pkgPath <- repoBinPath(path=path, type=type, Rversion=Rversion)
    if(grepl("mac.binary", type)) type <- "mac.binary"
    tools::write_PACKAGES(dir=pkgPath, type=type)
  })
  names(n) <- type
  return(n)
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

