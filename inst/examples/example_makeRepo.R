
# Specify list of packages to download
revolution <- c(CRAN="http://cran.revolutionanalytics.com")
pkgs <- "foreach"
pkgList <- pkgDep(pkgs, repos=revolution, type="source")

# Create temporary folder for miniCRAN
dir.create(pth <- file.path(tempdir(), "miniCRAN"))

# Make repo for source and win.binary
makeRepo(pkgList, path=pth, repos=revolution, download=TRUE, writePACKAGES=TRUE, type="source")
makeRepo(pkgList, path=pth, repos=revolution, download=TRUE, writePACKAGES=TRUE, type="win.binary")

# List all files in miniCRAN
list.files(pth, recursive = TRUE)

# Check for available packages
pkgAvail(repos=pth, type="source")
pkgAvail(repos=pth, type="win.binary")

# Plot dependency graph
p <- makeDepGraph(pkgList, repos=revolution)
library(igraph)
plot(p, vertex.size=10, edge.arrow.size=0.5, vertex.label.cex=0.8)

# Delete temporary folder
unlink(pth, recursive = TRUE)

