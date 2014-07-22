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
  pkg, availPkgs, repos=getOption("repos"), type="source", 
  path, suggests=TRUE, enhances=FALSE,
  includeBasePkgs=FALSE, ...)
{
  if(!require("igraph", quietly = TRUE)) stop("Package igraph is not installed")
  
  if(missing(availPkgs)) {
    availPkgs <- pkgAvail(repos=repos, type=type)
  }
  pkgs <- availPkgs
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


  # Build depends edge list for original pkg list, combined with top level suggests
  
  pkg_orig <- pkg

  if(suggests){
    edges1 <- pkgEdges(pkg, type=c("Suggests"), availPkgs)
    p_sug <- unique(unlist(
      tools::package_dependencies(pkg, db=availPkgs, which="Suggests", recursive=FALSE)
    ))
    pkg <- unique(c(p_sug, pkg))
  } 

  if(enhances){
    edges2 <- pkgEdges(pkg_orig, type=c("Enhances"), availPkgs)
    p_enh <- unique(unlist(
      tools::package_dependencies(pkg_orig, db=availPkgs, which="Enhances", recursive=FALSE)
    ))
    pkg <- unique(c(p_enh, pkg))
  } 
  
  p_dep <- unique(unlist(
    tools::package_dependencies(pkg, db=availPkgs, which=c("Imports", "Depends", "LinkingTo"), recursive=TRUE)
  ))
  pkg <- unique(c(p_dep, pkg))
  
  edges3 <- pkgEdges(pkg, type=c("Imports", "Depends", "LinkingTo"), availPkgs)

  edges <- edges3
  if(suggests){
    edges <- rbind(edges, edges1)
  }
  if(enhances) {
    edges <- rbind(edges, edges2)
  }
    
  if (nrow(edges) && !includeBasePkgs)
    edges <- edges[!(edges[["dep"]] %in% basePkgs()), ]
  
  vert <- unique(c(pkg_orig, edges[["dep"]], edges[["package"]]))
  ret <- igraph::graph.data.frame(d=edges, directed=TRUE, vertices = vert)
  class(ret) <- c("pkgDepGraph", "igraph")
  attr(ret, "pkgs") <- pkg_orig
  ret
}

