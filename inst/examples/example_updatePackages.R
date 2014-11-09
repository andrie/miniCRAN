### `oldPackages` and `updatePackages` require an existing miniCRAN repo

# Specify list of packages to download
revolution <- c(CRAN="http://cran.revolutionanalytics.com")
pkgs <- c("foreach")

pdb <- cranJuly2014

\dontrun{
  pdb <- pkgAvail(repos=revolution, type="source")
}

pkgList <- pkgDep(pkgs, availPkgs=pdb, repos=revolution, type="source", suggests=FALSE)
pkgList

\dontrun{
  # Create temporary folder for miniCRAN
  dir.create(pth <- file.path(tempdir(), "miniCRAN"))

  # create the miniCRAN directory structure but only add bin files
  makeRepo(pkgs, path=pth, repos=revolution, type="source", download=FALSE)
  makeRepo(pkgs, path=pth, repos=revolution, type="win.binary", download=TRUE)

  # download old source package version and create repo index
  addOldPackage(pkgs, path=pth, vers="1.4.0", type="source")
  # NOTE: older binary versions would need to be build from source

  # Check if updated packages are available
  oldPackages(path=pth, repos=revolution, type="source") # should need update
  oldPackages(path=pth, repos=revolution, type="win.binary") # should be current

  # Update available packages
  updatePackages(path=pth, repos=revolution, type="source") # should need update
  updatePackages(path=pth, repos=revolution, type="win.binary") # should be current

  # Delete temporary folder
  unlink(pth, recursive=TRUE)
}
