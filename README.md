miniCRAN
========

https://travis-ci.org/andrie/miniCRAN.svg?branch=master

R package to create internally consistent, mini version of CRAN

At the end of 2013, CRAN consisted of more than 5000 packages.  Many organisations need to maintain a private mirror of CRAN, but with only a subset of packages that are relevant to them.
 
`miniCRAN` makes this possible by recursively reading the dependency tree for a given set of packages, then downloading only this subset.
 
Important functions:

* Find package dependencies: `pkgDep()`
* Make repository (with or without downloading packages): `makeRepo()`

---

Installation:

    # Use `devtools` to install directly from github

    library(devtools)
    install_github("miniCRAN", username="andrie")
    
Example:

    # Determine and download the packages `ggplot2`, `plyr` and `reshape2`, including their dependencies:
    
    library("miniCRAN")
    
    pkgs <- c("ggplot2", "plyr", "reshape2")
    
    pkgDep(pkgs)
    
    ## NOT RUN
    
    pth <- "C:/Users/Andrie/Downloads/miniCRAN"
    makeRepo(pkgDep(pkgs), path=pth, download=TRUE)
    
    ## END NOT RUN
