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

