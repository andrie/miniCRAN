\name{pkgDep}
\alias{pkgDep}
\title{Retrieves package dependencies.}
\usage{
pkgDep(pkg, availPkgs, contriburl = getOption("repos"),
  type = getOption("pkgType"), ...)
}
\arguments{
  \item{pkg}{Character vector of packages.}

  \item{availPkgs}{Vector of available packages.  Defaults
  to reading this list from CRAN, using
  \code{\link{available.packages}}}

  \item{contriburl}{URL(s) of the 'contrib' sections of the
  repositories. Passed to \code{\link{available.packages}}}

  \item{type}{Passed to \code{\link{available.packages}}}

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
\dontrun{
pkgDep(c("ggplot2", "plyr", "reshape2"))
}
}
\seealso{
Other miniCRAN: \code{\link{makeRepo}};
\code{\link{pkgAvail}}
}

