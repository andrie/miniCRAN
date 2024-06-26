% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkgDep.R
\name{pkgDep}
\alias{pkgDep}
\title{Retrieves package dependencies.}
\usage{
pkgDep(
  pkg,
  availPkgs,
  repos = getOption("repos"),
  type = "source",
  depends = TRUE,
  suggests = TRUE,
  enhances = FALSE,
  includeBasePkgs = FALSE,
  Rversion = R.version,
  quiet = FALSE,
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

\item{depends}{If TRUE, retrieves \code{Depends}, \code{Imports} and \code{LinkingTo} dependencies
(non-recursively)}

\item{suggests}{If TRUE, retrieves Suggests dependencies (non-recursively)}

\item{enhances}{If TRUE, retrieves Enhances dependencies (non-recursively)}

\item{includeBasePkgs}{If TRUE, include base R packages in results}

\item{Rversion}{Version of R (only used if \code{type} is not \code{source}.) Defaults to \link{R.version}, but this can be specified as any of the following formats:
\itemize{
\item a character string with the two digit R version, e.g. "3.1"
\item a list with components \code{major} and \code{minor}
\item the result of \code{\link[=getRversion]{getRversion()}}
\item the result of \link{R.version}
}}

\item{quiet}{If TRUE, suppresses warnings}

\item{...}{Other arguments passed to \code{\link[=available.packages]{available.packages()}}}
}
\value{
character vector of package names
}
\description{
Performs recursive retrieve for \code{Depends}, \code{Imports} and \code{LinkLibrary}.
Performs non-recursive retrieve for \code{Suggests}.
}
\examples{

if (interactive()) {
  pkgDep(pkg = c("ggplot2", "plyr", "reshape2"),
         repos = c(CRAN = "https://cloud.r-project.org")
  )
  
  pdb <- cranJuly2014
  pdb <- pkgAvail(repos = c(CRAN = getOption("minicran.mran")))
  
  pkgDep(pkg = c("ggplot2", "plyr", "reshape2"), pdb)
  
}
}
\seealso{
Other dependency functions: 
\code{\link{basePkgs}()},
\code{\link{makeDepGraph}()},
\code{\link{plot.pkgDepGraph}()}
}
\concept{dependency functions}
