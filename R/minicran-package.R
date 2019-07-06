#' description
#'
#' At the end of 2014, CRAN consisted of more than 6,000 packages.  Many
#' organisations need to maintain a private mirror of CRAN, but with only a
#' subset of packages that are relevant to them.
#'
#' `miniCRAN` makes it possible to create an internally consistent repository
#' consisting of selected packages from CRAN-like repositories.  The user
#' specifies a set of desired packages, and miniCRAN recursively reads the
#' dependency tree for these packages, then downloads only this subset.
#'
#'
#' There are many reasons for not creating a complete mirror CRAN using `rsync`:
#'
#' * You may wish to mirror only a subset of CRAN, for security, legal
#' compliance or any other in-house reason
#'
#' * You may wish to restrict internal package use to a subset of public
#' packages, to minimize package duplication, or other reasons of coding
#' standards
#'
#' * You may wish to make packages available from public repositories other than
#' CRAN, e.g. BioConductor, r-forge, OmegaHat, etc.
#'
#' * You may wish to add custom in-house packages to your repository
#'
#' The ambition of `miniCRAN` is to eventually satisfy all of these
#' considerations.
#'
#' @section Making a private repo:
#'
#'   * [pkgAvail()]: Read from a local (or remote) CRAN-like repository and
#'   determine available packages.
#'
#'   * [pkgDep()]: Find (recursive) package dependencies.
#'
#'   * [makeRepo()] : Make a mini CRAN repository, by downloading packages (and
#'   their dependencies) and creating the appropriate file structure for a
#'   repository.  This allows you to use functions like
#'   [utils::available.packages()] and [utils::install.packages()] on your local
#'   repository.
#'
#'   This subset will be internally consistent, i.e. the following functions
#'   will work as expected:
#'
#'   * [utils::available.packages()]
#'
#'   * [utils::install.packages()]
#'
#'   The main function is [makeRepo()] - this will download all the required
#'   packages, with their dependencies, into the appropriate repository file
#'   structure, and then create the repository index (PACKAGES) file.
#'
#'
#'
#' @section Updating packages in a repo:
#'
#'   * [oldPackages()]: Indicates packages which have a (suitable) later version
#'   on the repositories * [updatePackages()]: Offers to download and install
#'   such packages
#'
#'
#'
#' @section Creating dependencies:
#'
#'   To get a recursive list of dependencies as well as a plot, use [pkgDep()]
#'   followed by  [makeDepGraph()].
#'
#'   * [pkgDep()]: Find (recursive) package dependencies.
#'
#'   * [makeDepGraph()]: Create graph of selected package dependencies.
#'
#'   * [plot.pkgDepGraph()]: Create a visualization of the dependency graph
#'
#'
#'
#' @section Package options:
#'
#'   \describe{ \item{\code{minicran.mran}}{preferred MRAN URL. Defaults to
#'   \url{https://mran.microsoft.com} for R versions 3.2.2 and greater. Versions
#'   earlier than 3.2.2 use HTTP instead of HTTPS.} }
#'
#' @name miniCRAN-package
#' @aliases miniCRAN minicran
#' @docType package
#' @keywords package
#' @importFrom graphics legend par title
#' @importFrom stats reshape setNames
#' @importFrom utils chooseCRANmirror compareVersion download.file flush.console
#'   installed.packages select.list
"_PACKAGE"




# data documentation ------------------------------------------------------

#' Stored version of available.packages()
#'
#' Copy of the result of [utils::available.packages()] on July 1, 2014.
#'
#' @docType data
#' @keywords datasets
#' @name cranJuly2014
#' @usage cranJuly2014
#' @format matrix
NULL
