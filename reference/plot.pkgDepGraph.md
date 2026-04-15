# Plots a package dependency graph.

Plots a package dependency graph.

## Usage

``` r
# S3 method for class 'pkgDepGraph'
plot(
  x,
  pkgsToHighlight,
  main = paste(attr(x, "pkgs"), collapse = ", "),
  legendPosition = c(-1.2, -1),
  shape = "circle",
  vertex.size = 8,
  cex = 1,
  ...
)
```

## Arguments

- x:

  Object to plot

- pkgsToHighlight:

  Optional character vector with names of package to highlight. If
  missing, defaults to packages used in original call to
  [`makeDepGraph()`](https://andrie.github.io/miniCRAN/reference/makeDepGraph.md)

- main:

  Title of plot

- legendPosition:

  Numeric vector of length 2, indicating (x, y) position of edge legend.
  Both values should be in the range `[-1; 1]`. If `NULL`, the edge
  legend is not displayed.

- shape:

  Shape of edge. See
  [`igraph::igraph.plotting()`](https://r.igraph.org/reference/plot.common.html).
  Could be "none", "circle", "square", ...

- vertex.size:

  Size of vertex shape.
  [`igraph::igraph.plotting()`](https://r.igraph.org/reference/plot.common.html)

- cex:

  Vertex label size.

- ...:

  Ignored

## See also

Other dependency functions:
[`basePkgs()`](https://andrie.github.io/miniCRAN/reference/basePkgs.md),
[`makeDepGraph()`](https://andrie.github.io/miniCRAN/reference/makeDepGraph.md),
[`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md)

## Examples

``` r
tags <- "chron"

# Plot using defaults

if (interactive()){
  pdb <- pkgAvail(
    repos = c(CRAN = getOption("minicran.mran")),
    type = "source"
  )
} else {
  pdb <- cranJuly2014
}


if (interactive()) {
  dg <- makeDepGraph(tags, availPkgs = pdb  , includeBasePkgs = FALSE,
                     suggests = TRUE, enhances = TRUE)
  
  set.seed(43);
  plot(dg)
  
  
  # Move edge legend to top left
  set.seed(42);
  plot(dg, legendPosition = c(-1, 1))
  
  # Change font size and shape size
  set.seed(42);
  plot(dg, legendPosition = c(-1, 1), vertex.size = 20,  cex = 0.5)
  
  
  # Move vertex legend to top right
  set.seed(42);
  plot(dg, legendPosition = c(1, 1), vertex.size = 20,  cex = 0.5)
}
```
