## ----make-repo-1---------------------------------------------------------
library(miniCRAN)
# Specify list of packages to download
pkgs <- c("foreach")

revolution <- c(CRAN="http://cran.revolutionanalytics.com")
pkgList <- pkgDep(pkgs, repos=revolution, type="source", suggests = FALSE, )
pkgList

## ----make-repo-2---------------------------------------------------------
# Create temporary folder for miniCRAN
dir.create(pth <- file.path(tempdir(), "miniCRAN"))

# Make repo for source and win.binary
makeRepo(pkgList, path=pth, repos=revolution, type="source")
makeRepo(pkgList, path=pth, repos=revolution, type="win.binary")

## ----make-repo-3---------------------------------------------------------
# List all files in miniCRAN
list.files(pth, recursive = TRUE, full.names = FALSE)

## ----make-repo-4---------------------------------------------------------
# Check for available packages
pkgAvail(repos=pth, type="win.binary")[, c(1:3, 5)]

## ----make-repo-5, include=FALSE------------------------------------------
# Delete temporary folder
unlink(pth, recursive = TRUE)

## ----init----------------------------------------------------------------
library(miniCRAN)

## ----pkgdep--------------------------------------------------------------
tags <- "chron"
pkgDep(tags, suggests=FALSE, enhances=FALSE, includeBasePkgs = TRUE)

pkgDep(tags, suggests = TRUE, enhances=FALSE)

pkgDep(tags, suggests = TRUE, enhances=TRUE)

## ----makeDepGraph, warning=FALSE-----------------------------------------
dg <- makeDepGraph(tags, includeBasePkgs=FALSE, suggests=TRUE, enhances=TRUE)
set.seed(1)
plot(dg, legendPosition = c(-1, 1), vertex.size=20)

## ----so-tags, warning=FALSE, fig.width=10, fig.height=10-----------------
tags <- c("ggplot2", "data.table", "plyr", "knitr", "shiny", "xts", "lattice")
pkgDep(tags, suggests = TRUE, enhances=FALSE)

dg <- makeDepGraph(tags, includeBasePkgs=FALSE, suggests=TRUE, enhances=TRUE)
set.seed(1)
plot(dg, legendPosition = c(-1, -1), vertex.size=10, cex=0.7)

