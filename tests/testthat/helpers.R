# helper functions for testing

# Returns TRUE if a URL can be accessed
is.online <- function(url = MRAN(), tryHttp = TRUE){
  readFromUrl <- function(url){
    z <- tryCatch(suppressWarnings(readLines(url, n = 1, warn = FALSE)),
                  error = function(e)e
    )
    !inherits(z, "error")
  }
  
  if(!readFromUrl(url)){
    if(grepl("^https://", url) && tryHttp){
      url <- sub("^https://", "http://", url) # for older versions of R
      if(!readFromUrl(url))
        return(FALSE)
    } else {
      return(FALSE)
    }
  }
  
  TRUE
}

# Interrupt the test if url can not be reached
skip_if_offline <- function(url = MRAN()){
  if (!is.online(url)) testthat::skip("offline")
}


# Use in unit tests to check if source or binary files are in repo
.checkForRepoFiles <- function(path, pkgList, prefix){
  ptn <- "tar\\.gz|zip|tgz"
  ff <- list.files(file.path(path, prefix), recursive = TRUE, pattern = ptn)
  if (length(ff) < length(pkgList)) return(FALSE)
  ret <- sapply(pkgList, function(x)any(grepl(x, ff)))
  if (all(ret > 0)) TRUE else {
    message(ret)
    FALSE
  }
}


mock.makeRepo <- function(...){
  mockery::stub(makeRepo, "download.packages", mock.download.packages)
  mockery::stub(makeRepo, "updateRepoIndex", mock.updateRepoIndex)
  makeRepo(...)
}

mock.addPackage <- function(...){
  # mockery::stub(addPackage, "makeRepo", mock.makeRepo)
  mockery::stub(addPackage, "download.packages", mock.download.packages, depth = 2)
  mockery::stub(addPackage, "updateRepoIndex", mock.updateRepoIndex, depth = 2)
  addPackage(...)
}

mock.updatePackages <- function(...){
  mockery::stub(updatePackages, "download.packages", mock.download.packages, depth = 3)
  mockery::stub(updatePackages, "updateRepoIndex", mock.updateRepoIndex, depth = 3)
  updatePackages(...)
}

mock.addLocalPackage <- function(...){
  mockery::stub(addLocalPackage, "updateRepoIndex", mock.updateRepoIndex)
  addLocalPackage(...)
}


mock.addOldPackage <- function(...){
  mockery::stub(addOldPackage, "download.packages", mock.download.packages, depth = 2)
  mockery::stub(addOldPackage, "updateRepoIndex", mock.updateRepoIndex, depth = 2)
  addOldPackage(...)
}


# Create sample repo from MRAN snapshot
.createSampleRepo <- function(MRAN, path, pkgs, Rversion = "3.1"){
  if (missing(MRAN)) MRAN <- MRAN("2014-10-15")
  if (missing(path)) path <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (missing(pkgs)) pkgs <- c("chron", "adaptivetau")
  
  pdb_source <- pkgAvail(repos = MRAN, type = "source", Rversion = Rversion)
  pdb_win    <- pkgAvail(repos = MRAN, type = "win.binary", Rversion = Rversion)
  pdb_mac    <- pkgAvail(repos = MRAN, type = "mac.binary", Rversion = Rversion)
  
  
  pkgList_source <- pkgDep(pkgs, availPkgs = pdb_source, repos = MRAN, 
                           type = "source", 
                           suggests = FALSE, Rversion = Rversion)
  mock.makeRepo(pkgList_source, path = path, repos = MRAN, 
           type = "source",
           quiet = TRUE, Rversion = Rversion)
  
  pkgList_win <- pkgDep(pkgs, availPkgs = pdb_win, repos = MRAN, 
                        type = "win.binary", 
                        suggests = FALSE, Rversion = Rversion)
  mock.makeRepo(pkgList_win, path = path, repos = MRAN, 
           type = "win.binary",
           quiet = TRUE, Rversion = Rversion)
  
  pkgList_mac <- pkgDep(pkgs, availPkgs = pdb_mac, repos = MRAN, 
                        type = "mac.binary", 
                        suggests = FALSE, Rversion = Rversion)
  mock.makeRepo(pkgList_mac, path = path, repos = MRAN, 
           type = "mac.binary",
           quiet = TRUE, Rversion = Rversion)
}

