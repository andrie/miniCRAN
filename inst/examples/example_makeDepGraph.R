


availPkgs <- cranJuly2014

\dontrun{
availPkgs <- pkgAvail(
  repos = c(CRAN = "http://mran.microsoft.com"),
  type = "source"
  )
}


# Create dependency graph using stored database of available packages
p <- makeDepGraph(
  c("ggplot2", "forecast"), 
  availPkgs = availPkgs
)

if(require(igraph)) plot(p)



\dontrun{
  # Create dependency graph using newly retrieved database from CRAN
  
  p <- makeDepGraph(
  c("ggplot2", "forecast"), 
  repos = c(CRAN = "http://mran.microsoft.com"), 
  type = "source"
)
if(require(igraph)) plot(p)
}
