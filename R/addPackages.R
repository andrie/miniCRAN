
readDescription <- function (file) {
  trimSpaces <- function(x){
    gsub("^\\s+|\\s+$", "", x)
  }
  
  prepDescription <- function(d){
    clean <- function(x)gsub("\n", " ", x)
    if(is.null(d$Depends)) d$Depends <- NA else d$Depends <- clean(d$Depends) 
    if(is.null(d$Imports)) d$Imports <- NA else d$Imports <- clean(d$Imports) 
    if(is.null(d$Suggests)) d$Suggests <- NA else d$Suggests <- clean(d$Suggests) 
    if(is.null(d$LinkingTo)) d$LinkingTo <- NA else d$LinkingTo <- clean(d$LinkingTo) 
    if(is.null(d$Enhances)) d$Enhances <- NA else d$Enhances <- clean(d$Enhances) 
    d
  }
  dcf <- read.dcf(file)
  dcfList <- setNames(as.list(dcf[1, ]), colnames(dcf))
  ret <- lapply(dcfList, trimSpaces)
  prepDescription(ret)
}




# from http://stackoverflow.com/questions/13163248/possible-to-override-the-blocking-of-a-packages-re-installation-after-it-has



readDescriptionGithub <- function(repo, username, branch="master", quiet=TRUE){
  if(!missing(username) && !is.null(username)) repo <- paste(username, repo, sep="/")
  pkg <- sprintf("https://github.com/%s/raw/%s/DESCRIPTION", repo, branch)
  ff <- tempfile()
  on.exit(unlink(ff))
  utils::setInternet2(use = TRUE)
  download.file(url=pkg, destfile=ff, quiet=quiet)
  readDescription(ff)
}



addPackage <- function(pdb, dcf, warnings=TRUE){
  pkgName <- dcf[["Package"]]
  pkgRow <- match(pkgName, rownames(pdb))
  newRow <- with(dcf, 
                 c(Package, Version, NA, Depends, Imports, LinkingTo, Suggests, Enhances, License, 
                   rep(NA, 8)))
  if(!is.na(pkgRow)){
    pdb[pkgRow, ] <- newRow
    if(warnings) {
      msg <- sprintf("Overwriting package information for: %s", pkgName)
      warning (msg)
    }
  } else {
    pdb <- rbind(pdb, newRow)
    rownames(pdb)[nrow(pdb)] <- pkgName
  }
  pdb
}

#' Add DESCRIPTION information from package on github.
#' 
#' Downloads the DESCRIPTION file from a package on github, parses the fields and adds (or replaces) a row in the available package database.
#' 
#' @param pdb Package database, usually the result of \code{\link{pkgAvail}} or \code{\link{available.packages}}
#' @param repo Character vector. Name of repository on github, e.g. \code{"RevolutionAnalytics/RRT"}
#' @param username Optional character vector. Name of repository on github, e.g. \code{"RevolutionAnalytics/RRT"}
#' @param branch name of branch, defaults to \code{"master"}
#' @export
#' @example \inst\examples\example-addPackage.R
addPackageGithub <- function(pdb=pkgAvail(), repo, username=NULL, branch="master"){
  desc <- readDescriptionGithub(repo=repo, username=username, branch=branch)
  addPackage(pdb, desc)
}


