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
                       , pdb){
  if(!p %in% rownames(pdb)) {
    quickdf(list(
      dep=character(0), 
      package=character(0), 
      type=character(0)
    ))
    
  } else {
    x <- cleanPkgField(pdb[p, type])
    quickdf(list(
      dep=x, 
      package=rep(p, length(x)), 
      type=rep(type, length(x))
    ))
  }
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
  pkg, pdb, repos=getOption("repos"), type="source", 
  path, 
  #   pkgs = pkgDep(pkg, repos=repos, type=type, pdb=pdb, ...), 
  includeBasePkgs=FALSE, ...)
{
  if(missing("pkg")) pkg <- pkgs
  if(!require("igraph", quietly = TRUE)) stop("Package igraph is not installed")
  
  if(missing(pdb)) {
    pdb <- pkgAvail(repos=repos, type=type)
  }
  pkgs = pkgDep(pkg, repos=repos, type=type, pdb=pdb)
  depPkgs <- pkgs[pkgs %in% rownames(pdb)]
  #   pkgs <- pdb[depPkgs, ]
  pkgs <- pdb
  if (!includeBasePkgs)
    baseOrRecPkgs <- rownames(installed.packages(priority="high"))
  allPkgs <- rownames(pkgs)
  if (!length(allPkgs))
    stop("no packages in specified repositories")
  
  
  
  pkgEdge <- function(p, type=c("Imports", "Depends", "LinkingTo"), pdb){
    do.call(rbind, lapply(type, function(t)addDepType(p, t, pdb=pdb)))
  }
  pkgEdges <- function(pp, type=c("Imports", "Depends", "LinkingTo"), pdb){
    do.call(rbind, lapply(pp, pkgEdge, type=type, pdb=pdb))
  }
  
  # Build suggests edge list for original pkg list
  
  edges1 <- pkgEdges(pkg, type=c("Suggests"), pdb)

  # Build depends edge list for original pkg list, combined with top level suggests
  
  p1 <- unique(unlist(
    tools::package_dependencies(pkg, db=pdb, which="Suggests", recursive=FALSE)
  ))
  p1 <- unique(c(p1, pkg))
  p2 <- unique(unlist(
    tools::package_dependencies(p1, db=pdb, which=c("Imports", "Depends", "LinkingTo"), recursive=TRUE)
  ))
  p2 <- unique(c(p1, p2))
  
  edges2 <- pkgEdges(p2, type=c("Imports", "Depends", "LinkingTo"), pdb)

  edges <- rbind(edges1, edges2)
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
  pch <- c(rep(19, length(plotColours)), rep(-8594, length(edgeColor)))
  #   legend(x=10, y=50, pch=c(rep(19, 2), rep(-8594, 5)), legend=letters[1:7])
  legend(x=-0.9, y=-0.9, legend=c("Dependencies", "Initial list", names(edgeColor)), 
         col=c(plotColours, edgeColor), 
         pch=pch, 
         cex=0.9)
  title(main)
}