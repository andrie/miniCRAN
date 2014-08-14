
# Specify list of packages to download
revolution <- c(CRAN="http://cran.revolutionanalytics.com")
pkgs <- "zoo"
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

# Delete temporary folder
unlink(pth, recursive = TRUE)

