#' Create dependency graph from available packages.
#' 
#' Each package is a node, and a dependency is an edge
#' 
#' @inheritParams pkgDep
#' @export
#' @family graph
#' @seealso pkgDep
makeDepGraph <- function(pkg, repos=getOption("repos"), type="source", path, pkgs = pkgDep(pkg, repos=repos, type=type)) {
  stopifnot(require(igraph))
  suggests.only <- FALSE
  keep.builtin <- FALSE
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
  edges <- lapply(rownames(pkgs), function(p) {
    if (!suggests.only) {
      deps <- cleanPkgField(pkgs[p, "Depends"])
      deps <- c(deps, cleanPkgField(pkgs[p, "Imports"]))
    } else {
      deps <- cleanPkgField(pkgs[p, "Suggests"])
    }
    deps <- unique(deps)
    if (length(deps) && !keep.builtin)
      deps <- deps[!(deps %in% baseOrRecPkgs)]
#     if (length(deps)) {
#       notFound <- ! (deps %in% nodes(depG))
#       #       if (any(notFound))
      #         depG <- addNode(deps[notFound], depG)
#       deps <- deps[!is.na(deps)]
      #       depG <- addEdge(from=p, to=deps, depG)
#     }
    data.frame(pkg=rep(p, length(deps)), dep=deps, stringsAsFactors=FALSE)
  }
  )
  #   if (dosize) {
  #     sizes <- getDownloadSizesBatched(makePkgUrl(pkgs))
  #     nodeData(depG, n=rownames(pkgs), attr="size") <- sizes
  #   }
  
  graph.data.frame(do.call(rbind, edges))

}

