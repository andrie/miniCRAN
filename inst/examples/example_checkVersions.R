### `checkVersions` and `add.packages.miniCRAN` require an existing miniCRAN repo

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

  # Make repo for source and win.binary
  makeRepo(pkgList, path=pth, repos=revolution, type="source")
  makeRepo(pkgList, path=pth, repos=revolution, type="win.binary")

  # Add other versions of a package (and assume these were added previously)
  pkgPathSrc <- file.path(path=pth, repoPrefix("source", R.version))
  pkgPathBin <- file.path(path=pth, repoPrefix("win.binary", R.version))

  oldPkgs <- c(file.path(revolution, repoPrefix("source", R.version),
                    "foreach_1.4.0.tar.gz"),
               file.path(revolution, repoPrefix("win.binary", R.version),
                         "foreach_1.4.0.zip"))
  download.file(oldPkgs[1], destfile=file.path(pkgPathSrc, "foreach_1.4.0.tar.gz"))
  download.file(oldPkgs[2], destfile=file.path(pkgPathBin, "foreach_1.4.0.zip"))

  # List package versions in the miniCRAN repo (produces warning about duplicates)
  pkgVersionsSrc <- checkVersions(pkgs, path=pth, type="source")
  pkgVersionsBin <- checkVersions(pkgs, path=pth, type="win.binary")

  # After inspecting package versions, remove old versions
  basename(pkgVersionsSrc)
  basename(pkgVersionsBin)
  file.remove(c(pkgVersionsSrc[1], pkgVersionsBin[1]))

  # Rebuild package index after adding/removing files
  tools::write_PACKAGES(pkgPathSrc, type="source")
  tools::write_PACKAGES(pkgPathBin, type="win.binary")

  # Add new packages (from CRAN) to the miniCRAN repo
  add.packages.miniCRAN("raster", path=pth, repos=revolution, type="source")
  add.packages.miniCRAN("raster", path=pth, repos=revolution, type="win.binary")

  # Delete temporary folder
  unlink(pth, recursive=TRUE)
}
