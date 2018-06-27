MRAN <- function(snapshot = NULL) {
  url <- getOption("minicran.mran")
  if (missing("snapshot") || is.null(snapshot)) {
    url
  } else {
    sprintf("%s/snapshot/%s", url, snapshot)
  }
}
CRAN <- function() getOption("repos")[1]
