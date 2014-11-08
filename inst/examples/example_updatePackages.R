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

  # manually create the miniCRAN directory structure in order to add old packages
  pkgPathSrc <- file.path(path=pth, repoPrefix("source", R.version))
  pkgPathBin <- file.path(path=pth, repoPrefix("win.binary", R.version))

  dir.create(pkgPathSrc, recursive=TRUE)
  dir.create(pkgPathBin, recursive=TRUE)

  # download old package version and create repo index
  oldPkgs <- c(file.path(revolution, repoPrefix("source", R.version),
                         "foreach_1.4.0.tar.gz"),
               file.path(revolution, repoPrefix("win.binary", R.version),
                         "foreach_1.4.0.zip"))
  download.file(oldPkgs[1], destfile=file.path(pkgPathSrc, "foreach_1.4.0.tar.gz"))
  download.file(oldPkgs[2], destfile=file.path(pkgPathBin, "foreach_1.4.0.zip"))

  tools::write_PACKAGES(pkgPathSrc, type="source")
  tools::write_PACKAGES(pkgPathBin, type="win.binary")

  # Check if updated packages are available
  oldPackages(path=pth, repos=revolution, type="source")
  oldPackages(path=pth, repos=revolution, type="win.binary")

  # Update available packages
  updatePackages(path=pth, repos=revolution, type="source")
  updatePackages(path=pth, repos=revolution, type="win.binary")

  # Delete temporary folder
  unlink(pth, recursive=TRUE)
}
