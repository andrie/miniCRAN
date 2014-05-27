#' Create include file for use with rsync.
#' 
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#' @param file Name of file where results are saved.  If NULL, the function returns a vector.
#' @family rsync
#' @export
#' @examples
#' pkgs <- c("ggplot2", "plyr", "reshape2")
#' makeRsyncInclude(pkgs, type="source", repos=c(CRAN="http://cran.revolutionanalytics.com"))
makeRsyncInclude <- function(pkg, file=NULL, pkgs=pkgDep(pkg, ...), ...){
  x <- paste0(pkgs, '*')
  if(exists("file") & !is.null(file)) {
    cat(x, file="dependency-list.txt", sep="\n")
  } else {
    x
  }

}
