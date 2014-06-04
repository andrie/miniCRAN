\name{makeRepo}
\alias{makeLibrary}
\alias{makeRepo}
\title{Downloads packages from CRAN to specified path and creates repository or library.}
\usage{
makeRepo(pkgs, path, type = "source", download = FALSE,
  writePACKAGES = TRUE)

makeLibrary(pkgs, path, type = "source")
}
\arguments{
  \item{pkgs}{Character vector of packages to download}

  \item{type}{Passed to \code{\link{download.packages}}}

  \item{download}{If TRUE downloads packages, otherwise
  just creates PACKAGES file}

  \item{writePACKAGES}{If TRUE, calls
  \code{\link[tools]{write_PACKAGES}} to update the
  repository PACKAGES file}

  \item{path}{Destination download path}
}
\description{
Given a list of packages, downloads to a specified
destination folder, then creates PACKAGES file.
}
\details{
The function \code{makeRepo} creates a repository, similar
in structure to CRAN.  It optionally updates the PACKAGES
file.  If done correctly, it is possible to use this folder
as a repository, i.e. it will support functions like
\code{install.packages}.

The function \code{makeLibrary} downloads the packages into
a single folder, i.e. similar to a library on a machine.

Uses \code{\link{download.packages}} and
\code{\link[tools]{write_PACKAGES}}
}
\seealso{
Other miniCRAN: \code{\link{getCranDescription}};
\code{\link{pkgAvail}}; \code{\link{pkgDep}}
}

