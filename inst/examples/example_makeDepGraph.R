
p <- makeDepGraph(
  c("ggplot2", "forecast"), 
  repos = c(CRAN="http://cran.revolutionanalytics.com"), 
  type="source"
  )
if(require(igraph)) plot(p)




availPkgs <- pkgAvail(
  repos = c(CRAN="http://cran.revolutionanalytics.com"),
  type="source"
  )

p <- makeDepGraph(
  c("ggplot2", "forecast"), 
  availPkgs = availPkgs
)

if(require(igraph)) plot(p)

