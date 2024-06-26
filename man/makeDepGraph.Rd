% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeDepGraph.R
\name{makeDepGraph}
\alias{makeDepGraph}
\title{Create dependency graph from available packages.}
\usage{
makeDepGraph(
  pkg,
  availPkgs,
  repos = getOption("repos"),
  type = "source",
  suggests = TRUE,
  enhances = FALSE,
  includeBasePkgs = FALSE,
  ...
)
}
\arguments{
\item{pkg}{Character vector of packages.}

\item{availPkgs}{Data frame with an element called \code{package}. The \code{package}
element is a vector of available packages.  Defaults to reading this list
from CRAN, using \code{\link[=available.packages]{available.packages()}}}

\item{repos}{URL(s) of the 'contrib' sections of the repositories, e.g.
\code{"https://cran.us.r-project.org"}. Passed to \code{\link[=available.packages]{available.packages()}}}

\item{type}{Possible values are (currently) "source", "mac.binary" and
"win.binary": the binary types can be listed and downloaded but not
installed on other platforms.  Passed to \code{\link[=download.packages]{download.packages()}}.}

\item{suggests}{If TRUE, retrieves Suggests dependencies (non-recursively)}

\item{enhances}{If TRUE, retrieves Enhances dependencies (non-recursively)}

\item{includeBasePkgs}{If TRUE, include base R packages in results}

\item{...}{Other arguments passed to \code{\link[=available.packages]{available.packages()}}}
}
\description{
Each package is a node, and a dependency is an edge
}
\examples{

if (interactive()) {
  availPkgs <- cranJuly2014
  
  availPkgs <- pkgAvail(
    repos = c(CRAN = "https://cloud.r-project.org"),
    type = "source"
  )
  
  
  # Create dependency graph using stored database of available packages
  p <- makeDepGraph(
    c("ggplot2", "forecast"),
    availPkgs = availPkgs
  )
  
  if(require(igraph)) plot(p)

  # Create dependency graph using newly retrieved database from CRAN
  
  p <- makeDepGraph(
    c("ggplot2", "forecast"),
    repos = c(CRAN = getOption("minicran.mran")),
    type = "source"
  )
  if(requireNamespace("igraph", quietly = TRUE)) {
    plot(p)
  } else {
    message("install package `igraph` to view dependency graph")
  }
  
}
}
\seealso{
\code{\link[=pkgDep]{pkgDep()}} to extract package dependencies

Other dependency functions: 
\code{\link{basePkgs}()},
\code{\link{pkgDep}()},
\code{\link{plot.pkgDepGraph}()}
}
\concept{dependency functions}
