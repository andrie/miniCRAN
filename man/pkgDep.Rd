\name{pkgDep}
\alias{pkgDep}
\title{Retrieves package dependencies.}
\usage{
  pkgDep(pkg,
    availPkgs = available.packages(contriburl = "http://cran.revolutionanalytics.com"))
}
\arguments{
  \item{pkg}{Character vector of packages.}

  \item{availPkgs}{Vector of available packages.  Defaults
  to reading this list from CRAN, using
  \code{\link{available.packages}}}
}
\description{
  Performs recursive retrieve for Depends, Imports and
  LinkLibrary. Performs non-recursive for Suggests.
}
\examples{
pkgDep(c("ggplot2", "plyr", "reshape2"))
}
\seealso{
  Other Package.dependency: \code{\link{makeRepo}}
}

