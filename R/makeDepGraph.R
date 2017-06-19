fastdf <- function (list) {
  rows <- unique(unlist(lapply(list, NROW)))
  stopifnot(length(rows) == 1)
  class(list) <- "data.frame"
  attr(list, "row.names") <- c(NA_integer_, -rows)
  list
}

addDepType <- function(p, type = c("Imports", "Depends", "LinkingTo", "Suggests"), pdb){
  if (!p %in% rownames(pdb)) {
    fastdf(list(
      dep = character(0),
      package = character(0),
      type = character(0)
    ))

  } else {
    x <- cleanPkgField(pdb[p, type])
    fastdf(list(
      dep = x,
      package = rep(p, length(x)),
      type = rep(type, length(x))
    ))
  }
}


igraphNotAvailableMessage <- "makeDepGraph requires igraph.  Install igraph and try again."

#' Create dependency graph from available packages.
#'
#' Each package is a node, and a dependency is an edge
#'
#' @inheritParams pkgDep
#' @inheritParams makeRepo
#'
#' @export
#' @family dependency functions
#' 
#' @seealso [pkgDep()] to extract package dependencies
#' 
#' @example /inst/examples/example_makeDepGraph.R
makeDepGraph <- function(
  pkg, availPkgs, repos = getOption("repos"), type = "source",
  suggests = TRUE, enhances = FALSE,
  includeBasePkgs = FALSE, ...)
{
  if (!requireNamespace("igraph")) stop(igraphNotAvailableMessage)
  
  if (missing(availPkgs)) {
    availPkgs <- pkgAvail(repos = repos, type = type)
  }
  pkgs <- availPkgs
  rownames(pkgs) <- as.vector(pkgs[, "Package"])
  allPkgs <- rownames(pkgs)
  if (!length(allPkgs))
    stop("no packages in specified repositories")



  pkgEdge <- function(p, type = c("Imports", "Depends", "LinkingTo"), pdb) {
    do.call(rbind, lapply(type, function(t)addDepType(p, t, pdb = pdb)))
  }
  pkgEdges <- function(pp, type = c("Imports", "Depends", "LinkingTo"), pdb) {
    do.call(rbind, lapply(pp, pkgEdge, type = type, pdb = pdb))
  }

  # Build depends edge list for original pkg list, combined with top level suggests

  pkg_orig <- pkg

  if (suggests) {
    edges1 <- pkgEdges(pkg, type = c("Suggests"), availPkgs)
    p_sug <- unique(unlist(
      tools::package_dependencies(pkg, db = availPkgs,
                                  which = "Suggests", recursive = FALSE)
    ))
    pkg <- unique(c(p_sug, pkg))
  }

  if (enhances) {
    edges2 <- pkgEdges(pkg_orig, type = c("Enhances"), availPkgs)
    p_enh <- unique(unlist(
      tools::package_dependencies(pkg_orig, db = availPkgs,
                                  which = "Enhances", recursive = FALSE)
    ))
    pkg <- unique(c(p_enh, pkg))
  }
  p_dep <- unique(unlist(
                  tools::package_dependencies(
                    pkg, db = availPkgs,
                    which = c("Imports", "Depends", "LinkingTo"), recursive = TRUE)
  ))
  pkg <- unique(c(p_dep, pkg))

  edges <- pkgEdges(pkg, type = c("Imports", "Depends", "LinkingTo"), availPkgs)

  if (suggests) {
    edges <- rbind(edges, edges1)
  }
  if (enhances) {
    edges <- rbind(edges, edges2)
  }
  nedges <- nrow(edges)
  if (nedges && !includeBasePkgs)
    edges <- edges[!(edges[["dep"]] %in% basePkgs()), ]

  vert <- unique(c(pkg_orig, edges[["dep"]], edges[["package"]]))
  ret <- igraph::graph.data.frame(d = edges, directed = TRUE, vertices = vert)
  class(ret) <- c("pkgDepGraph", "igraph")
  attr(ret, "pkgs") <- pkg_orig
  ret
}

