#' Scrape DESCRIPTION from CRAN for each pkg.
#'
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#' 
#' @importFrom tools CRAN_package_db
#' @export
#' 
#' @example /inst/examples/example_getCranDescription.R
getCranDescription <- function(pkg, repos = getOption("repos"), 
                               type = "source", 
                               pkgs = pkgDep(pkg, repos = repos, type = type)){

  pdb <- tools::CRAN_package_db()
  pdb[match(pkgs, pdb$Package), ]
}
