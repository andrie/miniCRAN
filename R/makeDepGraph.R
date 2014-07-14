quickdf <- function (list) {
  make_names <- function (x, prefix = "X") 
  {
    nm <- names(x)
    if (is.null(nm)) {
      nm <- rep.int("", length(x))
    }
    n <- sum(nm == "", na.rm = TRUE)
    nm[nm == ""] <- paste(prefix, seq_len(n), sep = "")
    nm
  }
  rows <- unique(unlist(lapply(list, NROW)))
  stopifnot(length(rows) == 1)
  names(list) <- make_names(list, "X")
  class(list) <- "data.frame"
  attr(list, "row.names") <- c(NA_integer_, -rows)
  list
}

addDepType <- function(p, type = c("Imports", "Depends", "LinkingTo", "Suggests")
                       , pkgs){
  x <- cleanPkgField(pkgs[p, type])
  quickdf(list(
    dep=x, 
    package=rep(p, length(x)), 
    type=rep(type, length(x))
  ))
}


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
  pkgs = pkgDep(pkg, repos=repos, type=type, availPkgs=availPkgs, ...), 
  includeBasePkgs=FALSE, ...)
{
  if(missing("pkg")) pkg <- pkgs
  if(!require("igraph", quietly = TRUE)) stop("Package igraph is not installed")
  
  if(missing(availPkgs)) {
    availPkgs <- pkgAvail(repos=repos, type=type)
  }
  depPkgs <- pkgs[pkgs %in% rownames(availPkgs)]
  pkgs <- availPkgs[depPkgs, ]
  if (!includeBasePkgs)
    baseOrRecPkgs <- rownames(installed.packages(priority="high"))
  allPkgs <- rownames(pkgs)
  if (!length(allPkgs))
    stop("no packages in specified repositories")
  
  
  
  pkgEdge <- function(p, type=c("Imports", "Depends", "LinkingTo")){
    do.call(rbind, lapply(type, function(t)addDepType(p, t, pkgs)))
  }
  pkgEdges <- function(pp, type=c("Imports", "Depends", "LinkingTo")){
    do.call(rbind, lapply(pp, pkgEdge, type=type))
  }
  
  pkgs <- pkgDep(pkg, depends=TRUE, enhances=FALSE, suggests=FALSE,
                    repos=repos, type=type, availPkgs=availPkgs)
  pkgs <- rownames(pkgs)
  edges1 <- pkgEdges(allPkgs, type=c("Imports", "Depends", "LinkingTo"))

  
  pkgs <- pkgDep(pkg, depends=FALSE, enhances=TRUE, suggests=FALSE, recursive=FALSE,
                    repos=repos, type=type, availPkgs=availPkgs)
  edges2 <- pkgEdges(pkg, type=c("Suggests", "Enhances"))

  
  pkgs <- pkgDep(pkg, depends=FALSE, enhances=FALSE, suggests=TRUE, recursive=FALSE,
                    repos=repos, type=type, availPkgs=availPkgs)
  edges3 <- pkgEdges(pkg, type=c("Suggests", "Enhances"))
  
  edges <- rbind(edges1, edges2, edges3)
  if (nrow(edges) && !includeBasePkgs)
    edges <- edges[!(edges[["dep"]] %in% baseOrRecPkgs), ]

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
  
  edgeColor <- c(Imports="red", Depends="orange", Suggests="grey80", Enhances="blue", LinkingTo="black")
  eColor <- edgeColor[get.edge.attribute(x, "type")]
  
  par(mai=rep(0.25, 4))
  
  plot(x, vertex.size=8, 
       edge.arrow.size=0.5, 
       edge.color=eColor,
       vertex.label.cex=0.7, 
       vertex.label.color="black", 
       vertex.color=vColor
  )
  legend(x=-0.9, y=-0.9, legend=c("Dependencies", "Initial list"), 
         col=c(plotColours, NA), pch=19, cex=0.9)
  text(-0.9, -0.75, expression("" %->% "depends on"), adj=0, cex=0.9)
  title(main)
}