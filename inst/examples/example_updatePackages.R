### `oldPackages` and `updatePackages` require an existing miniCRAN repo

# Specify list of packages to download
mirror <- c(CRAN = "https://cloud.r-project.org")
pkgs <- c("foreach")

pdb <- cranJuly2014

if (interactive()) {
  pdb <- pkgAvail(repos = mirror, type = "source")
  
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = mirror, type = "source", suggests = FALSE)
  pkgList
  
  # Create temporary folder for miniCRAN
  dir.create(pth <- file.path(tempdir(), "miniCRAN"))
  
  # create the miniCRAN directory structure but only add bin files
  makeRepo(pkgList, path = pth, repos = mirror, type = "source", download = FALSE)
  makeRepo(pkgList, path = pth, repos = mirror, type = "win.binary", download = TRUE)
  
  # download old source package version and create repo index
  oldVers <- data.frame(package = c("foreach", "codetools", "iterators"),
                        version = c("1.4.0", "0.2-7", "1.0.5"),
                        stringsAsFactors = FALSE)
  addOldPackage(pkgList, path = pth, repos = mirror, vers = oldVers$version, type = "source")
  # NOTE: older binary versions would need to be build from source
  
  # Check if updated packages are available
  oldPackages(path = pth, repos = mirror, type = "source") # should need update
  oldPackages(path = pth, repos = mirror, type = "win.binary") # should be current
  
  # Update available packages
  updatePackages(path = pth, repos = mirror, type = "source", ask = FALSE) # should need update
  updatePackages(path = pth, repos = mirror, type = "win.binary") # should be current
  
  # Delete temporary folder
  unlink(pth, recursive = TRUE)
}
