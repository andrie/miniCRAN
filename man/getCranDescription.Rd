\name{getCranDescription}
\alias{getCranDescription}
\title{Scrape DESCRIPTION from CRAN for each pkg.}
\usage{
getCranDescription(pkg, repos = getOption("repos"), type = "source", path,
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
Scrape DESCRIPTION from CRAN for each pkg.
}
\examples{
getCranDescription(c("igraph", "ggplot2", "XML"),
  repos=c(CRAN="http://cran.revolutionanalytics.com")
)
}
\seealso{
Other miniCRAN: \code{\link{makeLibrary}},
\code{\link{makeRepo}}; \code{\link{pkgAvail}};
\code{\link{pkgDep}}
}

