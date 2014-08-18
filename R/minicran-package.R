#
#  miniCRAN/R/minicran-package.R by Andrie de Vries  Copyright (C) 2014
#


#' Tools to create an internally consistent, mini version of CRAN with selected packages only.
#'
#' At the end of 2013, CRAN consisted of more than 5000 packages.  Many organisations need to maintain a private mirror of CRAN, but with only a subset of packages that are relevant to them.
#' 
#' \code{miniCRAN} makes this possible by recursively reading the dependency tree for a given set of packages, then downloading only this subset.
#' 
#' There are many reasons for not creating a complete mirror CRAN using rsync:
#'
#' \itemize{
#' \item You may wish to mirror only a subset of CRAN, for security, legal compliance or any other in-house reason
#' \item You may wish to restrict internal package use to a subset of public packages, to minimize package duplication, or other reasons of coding standards
#' \item You may wish to make packages available from public repositories other than CRAN, e.g. BioConductor, r-forge, OmegaHat, etc.
#' \item You may wish to add custom in-house packages to your repository
#' }
#' 
#' The ambition of miniCRAN is to eventually satisfy many of these considerations.  For example, the github version of miniCRAN already allows you to draw a dependency graph using packages on CRAN as well as github.  In due course I'd like to extend the package to also download packages from any public repository or private file location, as well as github packages.

#' 
#' Important functions:
#' 
#' \itemize{
#' \item \code{\link{pkgAvail}}: Read local repository and determine available packages
#' \item \code{\link{pkgDep}}: Find (recursive) package dependencies
#' \item \code{\link{makeDepGraph}}: Create graph of selected package dependencies
#' \item \code{\link{makeRepo}} : Make repository
#' }
#' 
#' This subset will be internally consistent, i.e. the following functions will work as expected:
#'
#' \itemize{
#' \item \code{\link{available.packages}}
#' \item \code{\link{install.packages}}
#' }
#'
#' The main function is \code{\link{makeRepo}} - this will download all the required packages, with their dependencies, into the appropriate repository file structure, and then create the repository index (PACKAGES) file.
#' 
#' To get a recursive list of dependencies, and a plot, use \code{\link{pkgDep}} and \code{\link{makeDepGraph}}.
#' 
#' 
#' 
#' 
#' @name miniCRAN-package
#' @aliases miniCRAN minicran
#' @docType package
#' @title Tools to create an internally consistent, mini version of CRAN with selected packages only.
#' @author Andrie de Vries \email{apdevries@@gmail.com}
#' @keywords package
#' @seealso \code{\link{minicran}}
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
