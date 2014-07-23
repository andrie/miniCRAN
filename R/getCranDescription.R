#' Scrape DESCRIPTION from CRAN for each pkg.
#' 
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#' @import XML
#' @export
#' @family miniCRAN
#' @example /inst/examples/example-getCranDescription.R
getCranDescription <- function(pkg, repos=getOption("repos"), type="source", path, pkgs = pkgDep(pkg, repos=repos, type=type)){
  
  getOne <- function(package){
    url <- sprintf("http://cran.r-project.org/web/packages/%s/index.html", package)
    x <- tryCatch({
      readHTMLTable(url, header=FALSE, which=1, stringsAsFactors=FALSE)
    }, error=function(e) e
    )
    if(inherits(x, "error")) {
      warning("Package ", package, " not found on CRAN")
      NULL
    } else {
      names(x) <- c("Field", "Value")
      x$Field <- gsub(":", "", x$Field)
      x$Package <- package
      x
    }
  }
  ret <- do.call(rbind, lapply(pkgs, getOne))
  ret <- reshape(ret, direction="wide", timevar="Field", idvar="Package", v.names="Value")
  names(ret) <- gsub("Value.", "", names(ret))
  rownames(ret) <- ret$Package
  ret
}