
.onLoad <- function(libname, pkgname) {
  mran.url <- if (getRversion() >= "3.2.2") {
    "https://packagemanager.posit.co/cran" ## use HTTPS
  } else {
    "http://packagemanager.posit.co/cran" ## use HTTP
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
