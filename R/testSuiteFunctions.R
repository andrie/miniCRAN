# Use in unit tests to check if source or binary files are in repo
.checkForRepoFiles <- function(path, pkgList, prefix){
  ptn <- "tar\\.gz|zip|tgz"
  ff <- list.files(file.path(path, prefix), recursive = TRUE, pattern = ptn)
  if(length(ff) < length(pkgList)) return(FALSE)
  ret <- sapply(pkgList, function(x)any(grepl(x, ff)))
  if(all(ret > 0)) TRUE else {
    message(ret)
    FALSE
  }
}


# Use in unit tests to copy sample repo to path
.copySampleRepo <- function(path){
  file.copy(
    from = list.dirs(system.file("inst/sample-repo", package="miniCRAN"), recursive = FALSE),
    to = path,
    recursive = TRUE
  )
}


# Create sample repo from MRAN snapshot
.createSampleRepo <- function(MRAN, path, pkgs){
  if(missing(MRAN)) MRAN <- c(CRAN="http://mran.revolutionanalytics.com/snapshot/2014-10-15")
  if(missing(path)) path <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if(missing(pkgs)) pkgs <- c("chron", "acss")
  
  pdb_source <- pkgAvail(repos=MRAN, type="source")
  pdb_win    <- pkgAvail(repos=MRAN, type="win.binary")
  pdb_mac    <- pkgAvail(repos=MRAN, type="mac.binary.mavericks")
  
  
  pkgList_source <- pkgDep(pkgs, availPkgs=pdb_source, repos=MRAN, type="source", suggests=FALSE)
  makeRepo(pkgList, path=path, repos=MRAN, type="source", quiet=TRUE)
  
  pkgList_win <- pkgDep(pkgs, availPkgs=pdb_win, repos=MRAN, type="win.binary", suggests=FALSE)
  makeRepo(pkgList, path=path, repos=MRAN, type="win.binary", quiet=TRUE)
  
  pkgList_mac <- pkgDep(pkgs, availPkgs=pdb_mac, repos=MRAN, type="mac.binary.mavericks", suggests=FALSE)
  makeRepo(pkgList, path=path, repos=MRAN, type="mac.binary.mavericks", quiet=TRUE)
}