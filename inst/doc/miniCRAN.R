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
plot(dg, legendPosEdge = c(-1, 1), legendPosVertex = c(1, 1), vertex.size=20)

## ----so-tags, warning=FALSE, fig.width=10, fig.height=10-----------------
tags <- c("ggplot2", "data.table", "plyr", "knitr", "shiny", "xts", "lattice")
pkgDep(tags, suggests = TRUE, enhances=FALSE)

dg <- makeDepGraph(tags, includeBasePkgs=FALSE, suggests=TRUE, enhances=TRUE)
plot(dg, legendPosEdge = c(-1, -1), legendPosVertex = c(1, -1), vertex.size=10, cex=1)

