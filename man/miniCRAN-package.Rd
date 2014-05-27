\docType{package}
\name{miniCRAN-package}
\alias{miniCRAN}
\alias{miniCRAN-package}
\alias{minicran}
\title{Tools to create an internally consistent, mini version of CRAN with selected packages only.}
\description{
Tools to create an internally consistent, mini version of
CRAN with selected packages only.
}
\details{
At the end of 2013, CRAN consisted of more than 5000
packages.  Many organisations need to maintain a private
mirror of CRAN, but with only a subset of packages that are
relevant to them.

\code{miniCRAN} makes this possible by recursively reading
the dependency tree for a given set of packages, then
downloading only this subset.

Important functions:

\itemize{ \item \code{\link{pkgDep}}: Find (recursive)
package dependencies \item \code{\link{makeRepo}} : Make
repository \item \code{\link{pkgAvail}}: Read local
repository and determine available packages \item
\code{\link{makeDepGraph}}: Create graph of selected
package dependencies }
}
\author{
Andrie de Vries \email{apdevries@gmail.com}
}
\seealso{
\code{\link{minicran}}
}
\keyword{package}

