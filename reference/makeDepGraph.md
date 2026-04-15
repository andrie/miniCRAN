# Create dependency graph from available packages.

Each package is a node, and a dependency is an edge

## Usage

``` r
makeDepGraph(
  pkg,
  availPkgs,
  repos = getOption("repos"),
  type = "source",
  suggests = TRUE,
  enhances = FALSE,
  includeBasePkgs = FALSE,
  ...
)
```

## Arguments

- pkg:

  Character vector of packages.

- availPkgs:

  Data frame with an element called `package`. The `package` element is
  a vector of available packages. Defaults to reading this list from
  CRAN, using
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- repos:

  URL(s) of the 'contrib' sections of the repositories, e.g.
  `"https://cran.us.r-project.org"`. Passed to
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- type:

  Possible values are (currently) "source", "mac.binary" and
  "win.binary": the binary types can be listed and downloaded but not
  installed on other platforms. Passed to
  [`download.packages()`](https://rdrr.io/r/utils/download.packages.html).

- suggests:

  If TRUE, retrieves Suggests dependencies (non-recursively)

- enhances:

  If TRUE, retrieves Enhances dependencies (non-recursively)

- includeBasePkgs:

  If TRUE, include base R packages in results

- ...:

  Other arguments passed to
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

## See also

[`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md) to
extract package dependencies

Other dependency functions:
[`basePkgs()`](https://andrie.github.io/miniCRAN/reference/basePkgs.md),
[`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md),
[`plot.pkgDepGraph()`](https://andrie.github.io/miniCRAN/reference/plot.pkgDepGraph.md)

## Examples

``` r
if (interactive()) {
  availPkgs <- cranJuly2014
  
  availPkgs <- pkgAvail(
    repos = c(CRAN = "https://cloud.r-project.org"),
    type = "source"
  )
  
  
  # Create dependency graph using stored database of available packages
  p <- makeDepGraph(
    c("ggplot2", "forecast"),
    availPkgs = availPkgs
  )
  
  if(require(igraph)) plot(p)

  # Create dependency graph using newly retrieved database from CRAN
  
  p <- makeDepGraph(
    c("ggplot2", "forecast"),
    repos = c(CRAN = getOption("minicran.mran")),
    type = "source"
  )
  if(requireNamespace("igraph", quietly = TRUE)) {
    plot(p)
  } else {
    message("install package `igraph` to view dependency graph")
  }
  
}
```
