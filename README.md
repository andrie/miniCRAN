miniCRAN
========

**Build status**

master: 
[![Build Status](https://travis-ci.org/andrie/miniCRAN.svg?branch=master)](https://travis-ci.org/andrie/miniCRAN)

dev: [![Build Status](https://travis-ci.org/andrie/miniCRAN.svg?branch=dev)](https://travis-ci.org/andrie/miniCRAN)




R package to create internally consistent, mini version of CRAN

## Introduction

At the end of 2013, CRAN consisted of more than 5000 packages.  Many organisations need to maintain a private mirror of CRAN, but with only a subset of packages that are relevant to them.
 
`miniCRAN` makes this possible by recursively reading the dependency tree for a given set of packages, then downloading only this subset.
 
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
install_github("miniCRAN", username="andrie")
```
    
## Example:

```r
# Determine and download the packages `ggplot2`, `plyr` and `reshape2`, including their dependencies:

library("miniCRAN")

pkgs <- c("ggplot2", "plyr", "reshape2")

pkgDep(pkgs)

pth <- file.path(tempdir, "miniCRAN")
makeRepo(pkgDep(pkgs), path=pth, download=TRUE)

```