#' Create dependency graph from available packages.
#' 
#' Each package is a node, and a dependency is an edge
#' 
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#' @export
#' @family graph
#' @seealso pkgDep plot.pkgDepGraph
#' @example /inst/examples/example-makeDepGraph.R
makeDepGraph <- function(
  pkg, availPkgs, repos=getOption("repos"), type="source", 
  path, 
  pkgs = pkgDep(pkg, repos=repos, type=type, availPkgs=availPkgs), 
  includeBasePkgs=FALSE)
{
  if(!require("igraph", quietly = TRUE)) stop("Package igraph is not installed")
  
  if(missing(availPkgs)) {
    availPkgs <- available.packages(contrib.url(repos, type=type))
  }
  depPkgs <- pkgs[pkgs %in% rownames(availPkgs)]
  pkgs <- availPkgs[depPkgs, ]
  if (!includeBasePkgs)
    baseOrRecPkgs <- rownames(installed.packages(priority="high"))
  allPkgs <- rownames(pkgs)
  if (!length(allPkgs))
    stop("no packages in specified repositories")
  
  pkgEdge <- function(p){
    deps <- c(
      cleanPkgField(pkgs[p, "Imports"]),
      cleanPkgField(pkgs[p, "Depends"]),
      cleanPkgField(pkgs[p, "LinkingTo"])
    )
    deps <- unique(deps)
    if (length(deps) && !includeBasePkgs)
      deps <- deps[!(deps %in% baseOrRecPkgs)]
    data.frame(pkg=rep(p, length(deps)), dep=deps, stringsAsFactors=FALSE)
  }
  
  edges <- do.call(rbind, lapply(allPkgs, pkgEdge))
  ret <- igraph::graph.data.frame(d=edges, directed=TRUE)
  class(ret) <- c("pkgDepGraph", "igraph")
  attr(ret, "pkgs") <- pkg
  ret
}

#' Plots a package dependency graph.
#' 
#' @param x pkgDepGraph object
#' @param pkgsToHighlight Optional character vector with names of package to hightlight. If missing, defaults to packages used in original call to \code{\link{makeDepGraph}}
#' @param main Title of plot
#' @param ... Ignored
#' @export
#' @seealso plot.pkgDepGraph
#' 
plot.pkgDepGraph <- function(
  x, pkgsToHighlight, 
  main="Package dependency graph", ...)
{
  if(!require("igraph", quietly = TRUE)) stop("Package igraph is not installed")
  class(x) <- "igraph"
  plotColours <- c("grey80", "orange")
  if(missing("pkgsToHighlight")) {
    pkgsToHighlight <- attr(x, "pkgs")
  }

  topLevel <- as.numeric(V(x)$name %in% pkgsToHighlight)
  vColor <- plotColours[1 + topLevel]
  
  par(mai=rep(0.25, 4))
  
  set.seed(50)
  plot(x, vertex.size=8, edge.arrow.size=0.5, 
       vertex.label.cex=0.7, vertex.label.color="black", 
       vertex.color=vColor)
  legend(x=0.9, y=-0.9, legend=c("Dependencies", "Initial list"), 
         col=c(plotColours, NA), pch=19, cex=0.9)
  text(0.9, -0.75, expression("" %->% "depends on"), adj=0, cex=0.9)
  title(main)
}