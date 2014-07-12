#
#  miniCRAN/R/minicran-package.R by Andrie de Vries  Copyright (C) 2014
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#


#' Tools to create an internally consistent, mini version of CRAN with selected packages only.
#'
#' At the end of 2013, CRAN consisted of more than 5000 packages.  Many organisations need to maintain a private mirror of CRAN, but with only a subset of packages that are relevant to them.
#' 
#' \code{miniCRAN} makes this possible by recursively reading the dependency tree for a given set of packages, then downloading only this subset.
#' 
#' Important functions:
#' 
#' \itemize{
#' \item \code{\link{pkgDep}}: Find (recursive) package dependencies
#' \item \code{\link{makeRepo}} : Make repository
#' \item \code{\link{pkgAvail}}: Read local repository and determine available packages
#' \item \code{\link{makeDepGraph}}: Create graph of selected package dependencies
#' }
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

