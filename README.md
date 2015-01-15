miniCRAN
========

**Build status**

master: 
[![Build Status](https://travis-ci.org/RevolutionAnalytics/miniCRAN.svg?branch=master)](https://travis-ci.org/RevolutionAnalytics/miniCRAN)
release:
[![Build Status](https://travis-ci.org/RevolutionAnalytics/miniCRAN.svg?branch=release)](https://travis-ci.org/RevolutionAnalytics/miniCRAN)
dev: [![Build Status](https://travis-ci.org/RevolutionAnalytics/miniCRAN.svg?branch=dev)](https://travis-ci.org/RevolutionAnalytics/miniCRAN)

Create a mini Version of CRAN Containing Only Selected Packages

## Introduction

At the end of 2014, CRAN consisted of more than 6,000 packages.  Many organisations need to maintain a private mirror of CRAN, but with only a subset of packages that are relevant to them.

`miniCRAN` makes it possible to create an internally consistent repository consisting of selected packages from CRAN-like repositories.  The user specifies a set of desired packages, and miniCRAN recursively reads the dependency tree for these packages, then downloads only this subset.  

## Important functions:

* Find package dependencies: `pkgDep()`
* Make repository (with or without downloading packages): `makeRepo()`


## Installation:

Get the stable version from CRAN:

```r
install.packages("miniCRAN")
library("miniCRAN")
```


Get a development version from github:

```r
# Use `devtools` to install directly from github
library(devtools)
install_github("RevolutionAnalytics/miniCRAN")
```
    
## Example:

```r
# Determine and download the packages `ggplot2`, `plyr` and `reshape2`, including their dependencies:
library("miniCRAN")
pkgs <- c("ggplot2", "plyr", "reshape2")
<<<<<<< HEAD
makeRepo(pkgDep(pkgs), path=file.path(tempdir, "miniCRAN"), download=TRUE)
=======
makeRepo(pkgDep(pkgs), path=file.path(tempdir(), "miniCRAN"), download=TRUE)
>>>>>>> dev
```
