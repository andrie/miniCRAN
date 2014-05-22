\name{makeDepGraph}
\alias{makeDepGraph}
\title{Create dependency graph from available packages.}
\usage{
makeDepGraph(repos, type = "source")
}
\arguments{
  \item{repos}{URL(s) of the 'contrib' sections of the
  repositories. Passed to \code{\link{available.packages}}}

  \item{type}{Passed to \code{\link{available.packages}}}
}
\description{
Each package is a node, and a dependency is an edge
}
\seealso{
pkgDep
}

