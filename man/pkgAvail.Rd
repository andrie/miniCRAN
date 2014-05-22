\name{pkgAvail}
\alias{pkgAvail}
\title{Reads available packages from CRAN repository.}
\usage{
pkgAvail(path, repos = getOption("repos"), type = getOption("pkgType"), ...)
}
\arguments{
  \item{path}{If supplied, locates available packages from
  local path}

  \item{repos}{}

  \item{type}{Passed to \code{\link{available.packages}}}

  \item{...}{Other arguments passed to
  \code{\link{available.packages}}}
}
\description{
This is a thin wrapper around
\code{\link{available.packages}}.  If the argument
\code{path} is supplied, then the function attempts to read
from a local repository, otherwise attempts to read from a
CRAN mirror at the \code{repos} url.
}
\seealso{
Other miniCRAN: \code{\link{makeRepo}};
\code{\link{pkgDep}}
}

