globals <- new.env(parent=emptyenv(), hash=TRUE)


.First.lib <- function(libname, pkgname) {
    globals$have_RCurl <- suppressWarnings(require("RCurl", quietly=TRUE))
}
.onLoad <- .First.lib
