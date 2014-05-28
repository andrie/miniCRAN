\name{makeRepo}
\alias{makeRepo}
\title{Downloads packages from CRAN to specified path.}
\usage{
makeRepo(pkgs, path, download = FALSE, type = "source")
}
\arguments{
  \item{pkgs}{Character vector of packages to download}

  \item{download}{If TRUE downloads packages, otherwise
  just creates PACKAGES file}

  \item{type}{Passed to \code{\link{download.packages}}}

  \item{path}{Destination download path}
}
\description{
Given a list of packages, downloads to a specified
destination folder, then creates PACKAGES file.
}
\details{
Uses \code{\link{download.packages}} and
\code{\link[tools]{write_PACKAGES}}
}
\seealso{
Other miniCRAN: \code{\link{getCranDescription}};
\code{\link{pkgAvail}}; \code{\link{pkgDep}}
}

