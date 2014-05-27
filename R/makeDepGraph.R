#' Create dependency graph from available packages.
#' 
#' Each package is a node, and a dependency is an edge
#' 
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#' @export
#' @family graph
#' @seealso pkgDep
#' @examples
#' p <- makeDepGraph(c("ggplot2", "forecast"), repos=c(CRAN="http://cran.revolutionanalytics.com"), type="source")
#' library(igraph)
#' plot(p)
makeDepGraph <- function(pkg, repos=getOption("repos"), type="source", path, pkgs = pkgDep(pkg, repos=repos, type=type)) {
  #   stopifnot(require(igraph))
  suggests.only <- FALSE
  keep.builtin <- TRUE
  dosize <- TRUE
  availPkgs <- available.packages(contrib.url(repos, type=type))
  #   depPkgs <- pkgDep(pkg=pkg, repos=repos, type=type)
  depPkgs <- pkgs[pkgs %in% rownames(availPkgs)]
  pkgs <- availPkgs[depPkgs, ]
  if (!keep.builtin)
    baseOrRecPkgs <- rownames(installed.packages(priority="high"))
  allPkgs <- rownames(pkgs)
  if (!length(allPkgs))
    stop("no packages in specified repositories")
  edges <- lapply(allPkgs, function(p) {
    deps <- cleanPkgField(pkgs[p, "Imports"])
    deps <- c(deps, cleanPkgField(pkgs[p, "Depends"]))
    deps <- c(deps, cleanPkgField(pkgs[p, "LinkingTo"]))
    deps <- unique(deps)
    if (length(deps) && !keep.builtin)
      deps <- deps[!(deps %in% baseOrRecPkgs)]
    data.frame(pkg=rep(p, length(deps)), dep=deps, stringsAsFactors=FALSE)
  }
  )
  edges <- do.call(rbind, edges)
  #   vertices <- data.frame(pkgs, stringsAsFactors=FALSE)
#   graph.data.frame(d=edges, vertices=vertices, directed=TRUE)
  graph.data.frame(d=edges, directed=TRUE)
  #   list(edges=edges, vertices=vertices)
  
}

