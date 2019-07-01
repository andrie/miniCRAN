
#' @importFrom XML readHTMLTable
oldGetCranDescription <- function(
  pkg, repos = getOption("repos"),
                                  type = "source",
                                  pkgs = pkgDep(pkg, repos = repos, type = type)
) {
  getOne <- function(package) {
    repos <- repos[[1]]
    if(!grepl("/$", repos)) repos <- paste0(repos, "/")

    url <- gsub("https://", "http://",
                sprintf("%sweb/packages/%s/index.html", repos, package)
    )
    x <- tryCatch({
      text <- paste(readLines(url), collapse = "\n")
      XML::readHTMLTable(text, header = FALSE, which = 1, stringsAsFactors = FALSE)
    }, error = function(e) e
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
  ret <- reshape(ret, direction = "wide", timevar = "Field", 
                 idvar = "Package", v.names = "Value")
  names(ret) <- gsub("Value.", "", names(ret))
  rownames(ret) <- ret$Package
  ret
}

#' Scrape DESCRIPTION from CRAN for each pkg.
#'
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#'
#' @export
#'
#' @example /inst/examples/example_getCranDescription.R
getCranDescription <- function(pkg, repos = getOption("repos"),
                               type = "source",
                               pkgs = pkgDep(pkg, repos = repos, type = type)) {

  if (getRversion() >= "3.4.1") {
    pdb <- tools::CRAN_package_db()
    pdb[match(pkgs, pdb$Package), ]
  } else {
    msg <- "In the next release of miniCRAN this function will not be supported in R-3.4.0 or earlier."
    warning(msg)
    oldGetCranDescription(pkg = pkg, repos = repos, type = type, pkgs = pkgs)
  }
}
