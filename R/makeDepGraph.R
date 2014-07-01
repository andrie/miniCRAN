#' Create dependency graph from available packages.
#' 
#' Each package is a node, and a dependency is an edge
#' 
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#' @export
#' @family graph
#' @seealso pkgDep
#' @example /inst/examples/example-makeDepGraph.R
makeDepGraph <- function(pkg, availPkgs, repos=getOption("repos"), type="source", path, 
                         pkgs = pkgDep(pkg, repos=repos, type=type, availPkgs=availPkgs), includeBasePkgs=FALSE) {
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
  graph.data.frame(d=edges, directed=TRUE)
}

