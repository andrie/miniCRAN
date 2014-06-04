\name{pkgDep}
\alias{pkgDep}
\title{Retrieves package dependencies.}
\usage{
pkgDep(pkg, availPkgs, repos = getOption("repos"),
  type = getOption("pkgType"), depends = TRUE, suggests = FALSE,
  enhances = FALSE, path, ...)
}
\arguments{
  \item{pkg}{Character vector of packages.}

  \item{availPkgs}{Vector of available packages.  Defaults
  to reading this list from CRAN, using
  \code{\link{available.packages}}}

  \item{repos}{URL(s) of the 'contrib' sections of the
  repositories. Passed to \code{\link{available.packages}}}

  \item{type}{Passed to \code{\link{available.packages}}}

  \item{depends}{If TRUE, retrieves Depends, Imports and
  LinkingTo dependencies (non-recursively)}

  \item{suggests}{If TRUE, retrieves Suggests dependencies
  (non-recursively)}

  \item{enhances}{If TRUE, retrieves Enhances dependencies
  (non-recursively)}

  \item{path}{Destination download path}

  \item{...}{Other arguments passed to
  \code{\link{available.packages}}}
}
\description{
Performs recursive retrieve for \code{Depends},
\code{Imports} and \code{LinkLibrary}. Performs
non-recursive retrieve for \code{Suggests}.
}
\details{
This
}
\examples{
pkgDep(pkg=
  c("ggplot2", "plyr", "reshape2"),
  repos=c(CRAN="http://cran.revolutionanalytics.com")
)
}
\seealso{
makeDepGraph

Other miniCRAN: \code{\link{getCranDescription}};
\code{\link{makeLibrary}}, \code{\link{makeRepo}};
\code{\link{pkgAvail}}
}

