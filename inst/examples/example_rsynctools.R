\dontrun{
  pkgs <- c("ggplot2", "plyr", "reshape2")
  makeRsyncInclude(pkgs, type="source", repos=c(CRAN="http://cran.revolutionanalytics.com"))
}
