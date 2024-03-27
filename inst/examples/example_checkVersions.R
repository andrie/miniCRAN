### `checkVersions` and `add.packages.miniCRAN` require an existing miniCRAN repo

# Specify list of packages to download
revolution <- c(CRAN = "https://cloud.r-project.org")
revolution
pkgs <- c("foreach")
pkgTypes <- c("source", "win.binary")

if (interactive()) {
  if (!is.online()) {
    message("p3m seems to be not available.  Check your internet connection.")
  } else {
    pdb <- pkgAvail(repos = revolution, type = "source")
  }
} else {
  pdb <- cranJuly2014
}


if (interactive()) {
  if (!is.online()) {
    message("p3m seems to be not available.  Check your internet connection.")
  } else {
    pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = revolution, type = "source", suggests = FALSE)
    pkgList
  }
}

# Create temporary folder for miniCRAN

if (interactive()) {
  if (!is.online()) {
    message("p3m seems to be not available.  Check your internet connection.")
  } else {
    dir.create(pth <- file.path(tempdir(), "miniCRAN"))
    
    # Make repo for source and win.binary
    makeRepo(pkgList, path = pth, repos = revolution, type = pkgTypes)
    
    # Add other versions of a package (and assume these were added previously)
    oldVers <- data.frame(
      package = c("foreach", "codetools", "iterators"),
      version = c("1.4.0", "0.2-7", "1.0.5"),
      stringsAsFactors = FALSE
    )
    pkgs <- oldVers$package
    addOldPackage(pkgs, path = pth, vers = oldVers$version, repos = revolution, type = "source")
    # NOTE: older binary versions would need to be build from source
    
    # List package versions in the miniCRAN repo (produces warning about duplicates)
    pkgVersionsSrc <- checkVersions(pkgs, path = pth, type = "source")
    pkgVersionsBin <- checkVersions(pkgs, path = pth, type = "win.binary")
    
    # After inspecting package versions, remove old versions
    basename(pkgVersionsSrc$source) # "foreach_1.4.0.tar.gz"  "foreach_1.4.2.tar.gz"
    basename(pkgVersionsBin$win.binary) # "foreach_1.4.0.zip"     "foreach_1.4.2.zip"
    file.remove(c(pkgVersionsSrc$source[1], pkgVersionsBin$win.binary[1]))
    
    # Rebuild package index after adding/removing files
    updateRepoIndex(pth, type = pkgTypes, Rversion = R.version)
    
    pkgAvail(pth, type = "source")
    
    # Add new packages (from CRAN) to the miniCRAN repo
    addPackage("Matrix", path = pth, repos = revolution, type = pkgTypes)
    
    # Delete temporary folder
    unlink(pth, recursive = TRUE)
  }
}
