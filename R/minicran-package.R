#
#  miniCRAN/R/minicran-package.R by Revolution Analytics  Copyright (C) 2014-2015
#


#' Create a Private Version of CRAN Containing Only Selected Packages
#'
#'@description
#'
#' At the end of 2014, CRAN consisted of more than 6,000 packages.  Many organisations need to maintain a private mirror of CRAN, but with only a subset of packages that are relevant to them.  
#' 
#' \code{miniCRAN} makes it possible to create an internally consistent repository consisting of selected packages from CRAN-like repositories.  The user specifies a set of desired packages, and miniCRAN recursively reads the dependency tree for these packages, then downloads only this subset.  
#' 
#' 
#' There are many reasons for not creating a complete mirror CRAN using \code{rsync}:
#'
#' \itemize{
#' \item You may wish to mirror only a subset of CRAN, for security, legal compliance or any other in-house reason
#' \item You may wish to restrict internal package use to a subset of public packages, to minimize package duplication, or other reasons of coding standards
#' \item You may wish to make packages available from public repositories other than CRAN, e.g. BioConductor, r-forge, OmegaHat, etc.
#' \item You may wish to add custom in-house packages to your repository
#' }
#' 
#' The ambition of \code{miniCRAN} is to eventually satisfy all of these considerations.
#' 
#' @section 1. Making a private repo:
#' 
#' \itemize{
#' \item \code{\link{pkgAvail}}: Read from a local (or remote) CRAN-like repository and determine available packages.
#' \item \code{\link{pkgDep}}: Find (recursive) package dependencies.
#' \item \code{\link{makeRepo}} : Make a mini CRAN repository, by downloading packages (and their dependencies) and creating the appropriate file structure for a repository.  This allows you to use functions like \code{\link[utils]{available.packages}} and \code{\link[utils]{install.packages}} on your local repository.
#' }
#' 
#' This subset will be internally consistent, i.e. the following functions will work as expected:
#'
#' \itemize{
#' \item \code{\link[utils]{available.packages}}
#' \item \code{\link[utils]{install.packages}}
#' }
#' 
#' The main function is \code{\link{makeRepo}} - this will download all the required packages, with their dependencies, into the appropriate repository file structure, and then create the repository index (PACKAGES) file.
#' 
#' 
#' 
#' @section 2. Updating packages in a repo:
#' 
#' \itemize{
#' \item \code{\link{oldPackages}}: Indicates packages which have a (suitable) later version on the repositories
#' \item \code{\link{updatePackages}}: Offers to download and install such packages
#' }
#' 
#' 
#' 
#' @section 3. Creating and visualising dependencies:
#' 
#' To get a recursive list of dependencies as well as a plot, use \code{\link{pkgDep}()} followed by  \code{\link{makeDepGraph}()}.
#' 
#' \itemize{
#' \item \code{\link{pkgDep}}: Find (recursive) package dependencies.
#' \item \code{\link{makeDepGraph}}: Create graph of selected package dependencies.
#' \item \code{\link{plot.pkgDepGraph}}: Create a visualization of the dependency graph
#' }
#' 
#' 
#' 
#' 
#' 
#' @name miniCRAN-package
#' @aliases miniCRAN minicran
#' @docType package
#' @author Andrie de Vries \email{adevries@@microsoft.com} with contributions from Alex Chubaty \email{alex.chubaty@@gmail.com}
#' @keywords package
#' @importFrom  graphics legend, par, title
#' @importFrom stats reshape setNames
#' @importFrom utils chooseCRANmirror compareVersion download.file flush.console installed.packages select.list
NULL



# data documentation ------------------------------------------------------

#' Stored version of available.packages()
#' 
#' Copy of the result of \code{\link[utils]{available.packages}} on July 1, 2014.
#' 
#' @docType data
#' @keywords datasets
#' @name cranJuly2014
#' @usage cranJuly2014
#' @format matrix
NULL
