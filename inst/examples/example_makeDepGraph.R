
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

  if(require(igraph)) plot(p)

  # Same thing without suggests

  p <- makeDepGraph(
    c("ggplot2", "forecast"),
    repos = c(CRAN = getOption("minicran.mran")),
    type = "source",
    suggests = FALSE
  )

  if(require(igraph)) plot(p)


  # Same thing without suggests and recursive dependencies

  p <- makeDepGraph(
    c("ggplot2", "forecast"),
    repos = c(CRAN = getOption("minicran.mran")),
    type = "source",
    suggests = FALSE,
    recursive = FALSE
  )

  if(requireNamespace("igraph", quietly = TRUE)) {
    plot(p)
  } else {
    message("install package `igraph` to view dependency graph")
  }
}
