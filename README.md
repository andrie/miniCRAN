miniCRAN
========

[![Build Status](https://travis-ci.org/RevolutionAnalytics/miniCRAN.svg?branch=master)](https://travis-ci.org/RevolutionAnalytics/miniCRAN)
[![Build Status](https://travis-ci.org/RevolutionAnalytics/miniCRAN.svg?branch=dev)](https://travis-ci.org/RevolutionAnalytics/miniCRAN)
[![](http://www.r-pkg.org/badges/version/miniCRAN)](http://www.r-pkg.org/pkg/miniCRAN)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/miniCRAN)](http://www.r-pkg.org/pkg/miniCRAN)
[![Coverage Status](https://img.shields.io/codecov/c/github/RevolutionAnalytics/miniCRAN/master.svg)](https://codecov.io/github/RevolutionAnalytics/miniCRAN?branch=master)

Create a mini version of CRAN containing only selected packages

## Introduction

At the end of 2014, CRAN consisted of more than 6,000 packages.  Many organisations need to maintain a private mirror of CRAN, but with only a subset of packages that are relevant to them.

`miniCRAN` makes it possible to create an internally consistent repository consisting of selected packages from CRAN-like repositories.  The user specifies a set of desired packages, and miniCRAN recursively reads the dependency tree for these packages, then downloads only this subset.  

## Important functions:

* Find package dependencies: `pkgDep()`
* Make repository (with or without downloading packages): `makeRepo()`
* Add additonal packages (and their dependencies) to existing repository: `addPackage()`
* Update the versions of packages currently in the repository: `updatePackages()`

## Installation:

Get the stable version from CRAN:

```r
install.packages("miniCRAN")
library("miniCRAN")
```


Get the latest stable development version from github:

```r
# Use `devtools` to install directly from github
library(devtools)
install_github("RevolutionAnalytics/miniCRAN")
```

## System requirements

The `miniCRAN` package itself doesn't introduce any system dependencies.  However, the package imports [`curl`](https://cran.r-project.org/package=curl) and `XML` packages. These have system requirements on `libxml2-devel`, `libcurl-devel` and `openssl-devel`.

On systems with the `rpm` package manager (Red Hat, CentOS) try:

```bash
yum install libcurl-devel libxml2-devel openssl-devel
```

On systems with the `atp` package manager (Debian, Ubuntu) try:

```bash
apt-get install libcurl4-openssl-dev libxml2-devel openssl-devel
```

    
## Example:

```r
# Determine and download the packages `ggplot2`, `plyr` and `reshape2`, including their dependencies:

library("miniCRAN")
pkgs <- c("ggplot2", "plyr", "reshape2")
makeRepo(pkgDep(pkgs), path=file.path(tempdir(), "miniCRAN"), download=TRUE)
```

## Code of conduct

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/). For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
