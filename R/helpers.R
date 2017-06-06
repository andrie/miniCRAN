#' Get the path to the repo directory containing the package files.
#' 
#' @note Not all versions of R are compatible with with all package types (e.g., \code{mac.binary.el-capitan} in only valid for R > 3.4.0).
#'
#' @param Rversion Version of R. Can be specified as a character string with the two digit R version, e.g. "3.1".  Defaults to \code{\link{R.version}}
#'
#' @param type  character, indicating the type of package to download and install. See \code{\link{install.packages}}.
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
#'      \item{macosx/el-capitan/contrib/version}
#'      \itemize{
#'        \item{PACKAGES}
#'      }
#'      \item{macosx/leopard/contrib/version}
#'      \itemize{
#'        \item{PACKAGES}
#'      }
#'      \item{macosx/mavericks/contrib/version}
#'      \itemize{
#'        \item{PACKAGES}
#'      }
#'    }
#'  }
#' }
#'
#' @return The filepath to the package files directory.
#'
repoPrefix <- function(type, Rversion) {
  Rversion <- twodigitRversion(Rversion)
  
  if ((type == "mac.binary.el-capitan") && (numeric_version(Rversion) < "3.4")) {
    warning("Type mac.binary.el-capitan only valid for R >= 3.4")
  } else if ((type == "mac.binary.mavericks") && (numeric_version(Rversion) >= "3.4")) {
    warning("Type mac.binary.mavericks only valid for R < 3.4")
  }
  
  switch(
    type,
    "source" = "src/contrib",
    "win.binary" = sprintf("bin/windows/contrib/%s", Rversion),
    "mac.binary" = sprintf("bin/macosx/contrib/%s", Rversion),
    "mac.binary.el-capitan" = sprintf("bin/macosx/el-capitan/contrib/%s", Rversion),
    "mac.binary.leopard" = sprintf("bin/macosx/leopard/contrib/%s", Rversion),
    "mac.binary.mavericks" =  sprintf("bin/macosx/mavericks/contrib/%s", Rversion),
    stop("Type ", type, "not recognised.")
  )
}

#' Construct path to full binary location
#' @inheritParams makeRepo
#' @inheritParams repoPrefix
repoBinPath <- function(path, type, Rversion){
  normalizePath(file.path(path, repoPrefix(type, Rversion)), mustWork = FALSE, winslash = "/")
}



#' Get a two-digit version of the R version
#'
#' @param R Either a list of the format \code{\link{R.version}}, a character string (e.g., \code{"3.1.2"}), or a numeric version of the type \code{\link{R_system_version}}.
#'
#' @return A character string representing the two-digit R version.
#'
#' @importFrom methods is
#'
twodigitRversion <- function(R=R.version){
  if ("simple.list" %in% is(R)) {
    paste(R$major, strsplit(R$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
  } else if ("R_system_version" %in% is(R)) {
    paste(strsplit(as.character(R), ".", fixed=TRUE)[[1L]][1L:2L], collapse=".")
  } else if (is.character(R)) {
    paste(strsplit(R, ".", fixed=TRUE)[[1L]][1L:2L], collapse=".")
  }
}
