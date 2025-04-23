#' Obtains DESCRIPTION metadata from CRAN for each package.
#'
#' This is a wrapper around `tools::CRAN_package_db` and may be
#' deprecated in future versions of the package.
#'
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#'
#' @export
#'
#' @example /inst/examples/example_getCranDescription.R
getCranDescription <- function(
  pkg,
  repos = getOption("repos"),
  type = "source",
  pkgs = pkgDep(pkg, repos = repos, type = type)
) {
  if (getRversion() >= "3.4.1") {
    pdb <- tools::CRAN_package_db()
    pdb[match(pkgs, pdb$Package), ]
  } else {
    msg <- "This function is not available in R-3.4.0 or earlier."
    stop(msg)
  }
}
