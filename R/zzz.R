# globals <- new.env(parent=emptyenv(), hash=TRUE)
# globals$have_RCurl <- require("RCurl")

.onLoad <- function(libname, pkgname) {
  mran.url <- if (getRversion() >= "3.2.2") {
    "https://mran.microsoft.com" ## use HTTPS
  } else {
    "http://mran.microsoft.com" ## use HTTP
  }

  ## set options using the approach used by devtools
  opts <- options()
  opts.miniCRAN <- list(
    minicran.mran = mran.url
  )
  toset <- !(names(opts.miniCRAN) %in% names(opts))
  if (any(toset)) options(opts.miniCRAN[toset])

  invisible()
}
