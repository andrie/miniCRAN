
# Specify list of packages to download
mirror <- c(CRAN = "https://cloud.r-project.org")
pkgs <- c("foreach")

if (interactive()) {
  pdb <- cranJuly2014
  
  pdb <- pkgAvail(
    repos = c(CRAN = getOption("minicran.mran")),
    type = "source"
  )
  
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = mirror,
                    type = "source", suggests = FALSE)
  pkgList
  
  
  # Create temporary folder for miniCRAN
  dir.create(pth <- file.path(tempdir(), "miniCRAN"))
  
  # Make repo for source and win.binary
  makeRepo(pkgList, path = pth, repos = mirror, type = "source")
  
  # List all files in miniCRAN
  list.files(pth, recursive = TRUE)
  
  # Check for available packages
  pkgAvail(repos = pth, type = "source")
  
  # Repeat process for windows binaries
  makeRepo(pkgList, path = pth, repos = mirror, type = "win.binary")
  pkgAvail(repos = pth, type = "win.binary")
  
  # Delete temporary folder
  unlink(pth, recursive = TRUE)
}