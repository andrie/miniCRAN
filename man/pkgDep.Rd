\name{pkgDep}
\alias{pkgDep}
\title{Retrieves package dependencies.}
\usage{
  pkgDep(pkg, availPkgs = available.packages())
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
\seealso{
  Other Package.dependency: \code{\link{makeRepo}}
}

