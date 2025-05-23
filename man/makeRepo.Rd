% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeRepo.R
\name{makeRepo}
\alias{makeRepo}
\alias{updateRepoIndex}
\title{Downloads packages from CRAN to specified path and creates a local
repository.}
\usage{
makeRepo(
  pkgs,
  path,
  repos = getOption("repos"),
  type = "source",
  Rversion = R.version,
  download = TRUE,
  writePACKAGES = TRUE,
  filters = NULL,
  quiet = FALSE
)

updateRepoIndex(path, type = "source", Rversion = R.version)
}
\arguments{
\item{pkgs}{Character vector of packages to download}

\item{path}{Destination download path. This path is the root folder of your
new repository.}

\item{repos}{URL(s) of the 'contrib' sections of the repositories, e.g.
\code{"https://cran.us.r-project.org"}. Passed to \code{\link[=available.packages]{available.packages()}}}

\item{type}{Possible values are (currently) "source", "mac.binary" and
"win.binary": the binary types can be listed and downloaded but not
installed on other platforms.  Passed to \code{\link[=download.packages]{download.packages()}}.}

\item{Rversion}{Version of R (only used if \code{type} is not \code{source}.) Defaults to \link{R.version}, but this can be specified as any of the following formats:
\itemize{
\item a character string with the two digit R version, e.g. "3.1"
\item a list with components \code{major} and \code{minor}
\item the result of \code{\link[=getRversion]{getRversion()}}
\item the result of \link{R.version}
}}

\item{download}{If TRUE downloads packages.}

\item{writePACKAGES}{If TRUE, calls \code{\link[=write_PACKAGES]{write_PACKAGES()}} to update the
repository PACKAGES file.}

\item{filters}{passed to \link[utils:available.packages]{utils::available.packages}}

\item{quiet}{If TRUE, suppress status messages (if any), and the progress bar
during download.}
}
\value{
character vector of downloaded package files
}
\description{
Given a list of packages, downloads these packages to a specified destination
folder using the required CRAN folder structure, and finally creates the
PACKAGES index file.  Since the folder structure mimics the required
structure and files of a CRAN repository, it supports functions like
\code{\link[utils:install.packages]{utils::install.packages()}}.
}
\note{
Internally makes use of \code{\link[utils:download.packages]{utils::download.packages()}} and
\code{\link[=write_PACKAGES]{write_PACKAGES()}}
}
\section{Repo folder structure}{


A repository has two main folders, one for source packages, and the other
for binary packages.  Inside the binary package folder, \code{bin}, you will find
subfolders for Windows as well as the various OSX binaries.

\code{+- Root}

\code{...+- src/contrib}

\code{......+- PACKAGES}

\code{..+- bin}

\code{.......+- windows/contrib/version}

\code{..........+- PACKAGES}

\code{.......+-  macosx/contrib/version}

\code{..........+-  PACKAGES}

\code{.......+-  macosx/mavericks/contrib/version}

\code{..........+-  PACKAGES}

\code{.......+-  macosx/leopard/contrib/version}

\code{..........+- PACKAGES}
}

\examples{

# Specify list of packages to download
mirror <- c(CRAN = "https://cloud.r-project.org")
pkgs <- c("foreach")

if (interactive()) {
  pdb <- cranJuly2014
  
  pdb <- pkgAvail(
    repos = c(CRAN = getOption("minicran.mran")),
    type = "source"
  )
  
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = mirror,
                    type = "source", suggests = FALSE)
  pkgList
  
  
  # Create temporary folder for miniCRAN
  dir.create(pth <- file.path(tempdir(), "miniCRAN"))
  
  # Make repo for source and win.binary
  makeRepo(pkgList, path = pth, repos = mirror, type = "source")
  
  # List all files in miniCRAN
  list.files(pth, recursive = TRUE)
  
  # Check for available packages
  pkgAvail(repos = pth, type = "source")
  
  # Repeat process for windows binaries
  makeRepo(pkgList, path = pth, repos = mirror, type = "win.binary")
  pkgAvail(repos = pth, type = "win.binary")
  
  # Delete temporary folder
  unlink(pth, recursive = TRUE)
}
}
\seealso{
Other update repo functions: 
\code{\link{addOldPackage}()},
\code{\link{addPackage}()},
\code{\link{checkVersions}()},
\code{\link{updatePackages}()}
}
\concept{update repo functions}
