## ----init----------------------------------------------------------------
library("miniCRAN")

## ----pkgdep--------------------------------------------------------------
tags <- "chron"
pkgDep(tags)

## ----makeDepGraph, warning=FALSE-----------------------------------------
dg <- makeDepGraph(tags, enhances=TRUE)
set.seed(1)
plot(dg, legendPosition = c(-1, 1), vertex.size=20)

## ----so-tags, warning=FALSE, fig.width=10, fig.height=10-----------------
tags <- c("ggplot2", "data.table", "plyr", "knitr", "shiny", "xts", "lattice")
pkgDep(tags, suggests = TRUE, enhances=FALSE)

dg <- makeDepGraph(tags, enhances=TRUE)
set.seed(1)
plot(dg, legendPosition = c(-1, -1), vertex.size=10, cex=0.7)

