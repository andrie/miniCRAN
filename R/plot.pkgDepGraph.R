#' Plots a package dependency graph.
#' 
#' @param x pkgDepGraph object
#' @param pkgsToHighlight Optional character vector with names of package to hightlight. If missing, defaults to packages used in original call to \code{\link{makeDepGraph}}
#' @param main Title of plot
#' @param legendPosVertex Numeric vector of length 2, indicating (x, y) position of vertex legend. Both values should be in the range [-1; 1].  If NULL, the vertex legend is not displayed.
#' @param legendPosEdge Numeric vector of length 2, indicating (x, y) position of edge legend. Both values should be in the range [-1; 1].  If NULL, the edge legend is not displayed.
#' @param shape Shape of edge.  See \code{\link[igraph]{igraph.plotting}}. Could be "none", "circle", "square", ...
#' @param vertex.size Size of vertex shape. See \code{\link[igraph]{igraph.plotting}}
#' @param cex Vertex label size.
#' @param ... Ignored
#' @export
#' @seealso \code{\link{makeDepGraph}}
#' @example \inst\examples\example_plot.pkgDepGraph.R
#' 
plot.pkgDepGraph <- function(
  x, pkgsToHighlight, 
  main="Package dependency graph", 
  legendPosVertex = c(-1, -1),
  legendPosEdge = c(1, -1),
  shape="circle",
  vertex.size = 8,
  cex=1,
  ...)
{
  if(!require("igraph", quietly = TRUE)) stop("Package igraph is not installed")
  class(x) <- "igraph"
  plotColours <- c("grey80", "orange")
  if(missing("pkgsToHighlight")) {
    pkgsToHighlight <- attr(x, "pkgs")
  }
  
  topLevel <- as.numeric(V(x)$name %in% pkgsToHighlight)
  vColor <- plotColours[1 + topLevel]
  vFont <- 1 + topLevel
  vShape <- c("none", shape)[1 + topLevel]
  
  edgeColor <- c(Imports="red", Depends="orange", Suggests="grey80", Enhances="blue", LinkingTo="black")
  eColor <- edgeColor[get.edge.attribute(x, "type")]
  
  par(mai=rep(0.25, 4))
  
  plot(x, vertex.size=vertex.size, 
       edge.arrow.size=0.5, 
       edge.color=eColor,
       vertex.label.cex=cex, 
       vertex.label.color="black", 
       vertex.color=vColor,
       vertex.shape=vShape,
       vertex.label.font=vFont
  )
  pch1 <- rep(19, length(plotColours))
  pch2 <- rep(-8594, length(edgeColor))
  just <- function(x)0.5*(x+1)
  
  # Vertex legend
  if(!is.null(legendPosVertex)){
    legend(x=legendPosVertex[1], y=legendPosVertex[2], 
           xjust=just(legendPosVertex[1]), 
           yjust=just(legendPosVertex[2]),
           legend=c("Dependencies", "Initial list"), 
           col=c(plotColours), 
           text.font=1:2,
           pch=pch1, 
           y.intersp=0.75,
           cex=cex)
  }
  
  # Edge legend
  if(!is.null(legendPosEdge)){
    legend(x=legendPosEdge[1], 
           y=legendPosEdge[2], 
           xjust=just(legendPosEdge[1]), 
           yjust=just(legendPosEdge[2]),
           legend=names(edgeColor), 
           col=edgeColor, 
           pch=pch2, 
           y.intersp=0.75,
           cex=cex)
  }
  title(main)
}