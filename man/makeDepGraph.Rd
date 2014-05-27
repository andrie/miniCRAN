\name{makeDepGraph}
\alias{makeDepGraph}
\title{Create dependency graph from available packages.}
\usage{
makeDepGraph(pkg, repos = getOption("repos"), type = "source", path,
  pkgs = pkgDep(pkg, repos = repos, type = type))
}
\arguments{
  \item{pkg}{Character vector of packages.}

  \item{repos}{URL(s) of the 'contrib' sections of the
  repositories. Passed to \code{\link{available.packages}}}

  \item{type}{Passed to \code{\link{available.packages}}}

  \item{path}{Destination download path}

  \item{pkgs}{Character vector of packages to download}
}
\description{
Each package is a node, and a dependency is an edge
}
\seealso{
pkgDep
}

