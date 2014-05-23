#' Create include file for use with rsync.
#' 
#' @inheritParams pkgDep
#' @family rsync
#' @export
#' @examples
#' pkgs <- c("ggplot2", "plyr", "reshape2")
#' makeRsyncInclude(pkgs)
makeRsyncInclude <- function(pkg, pkgs=pkgDep(pkg)){
  x <- paste0(x, '*')
  write.table(x, file="dependency-list.txt", row.names=FALSE, quote=FALSE, col.names=FALSE)
}
