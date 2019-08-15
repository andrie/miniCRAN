pkgFileExt <- function(type) {
  switch(
    type,
    "source" = ".tar.gz",
    "win.binary" = ".zip",
    "mac.binary" = ".tgz",
    "mac.binary.mavericks" = ".tgz",
    "mac.binary.leopard" = ".tgz",
    "mac.binary.el-capitan" = ".tgz",
    stop("Type ", type, " not recognised.")
  )
}


getPkgVersFromFile <- function(file) {
  file <- grep("\\.(tar\\.gz|zip|tgz)$", basename(as.character(file)), value = TRUE)
  if (length(file)) {
    file <- sapply(strsplit(file, "\\.tar\\.gz"), "[[", 1)
    file <- sapply(strsplit(file, "\\.zip"), "[[", 1)
    file <- sapply(strsplit(file, "\\.tgz"), "[[", 1)
    pkg  <- sapply(strsplit(file, "_"), "[[", 1)
    vers <- sapply(strsplit(file, "_"), "[[", 2)
    df <- data.frame(package = pkg, version = vers, stringsAsFactors = FALSE, row.names = NULL)
    df <- df[order(df$package),]
    row.names(df) <- seq_len(nrow(df))
    df
  } else {
    data.frame(package = character(0), version = character(0), stringsAsFactors = FALSE)
  }
}



readDescription <- function(file) {
  stopifnot(file.exists(file))
  trimSpaces <- function(x) {
    gsub("^\\s+|\\s+$", "", x)
  }

  prepDescription <- function(d) {
    clean <- function(x) gsub("\n", " ", x)
    if (is.null(d$Depends)) d$Depends <- NA else d$Depends <- clean(d$Depends)
    if (is.null(d$Imports)) d$Imports <- NA else d$Imports <- clean(d$Imports)
    if (is.null(d$Suggests)) d$Suggests <- NA else d$Suggests <- clean(d$Suggests)
    if (is.null(d$LinkingTo)) d$LinkingTo <- NA else d$LinkingTo <- clean(d$LinkingTo)
    if (is.null(d$Enhances)) d$Enhances <- NA else d$Enhances <- clean(d$Enhances)
    d
  }
  dcf <- read.dcf(file)
  dcfList <- setNames(as.list(dcf[1, ]), colnames(dcf))
  ret <- lapply(dcfList, trimSpaces)
  prepDescription(ret)
}



addPackageListing <- function(pdb = pkgAvail(), dcf, warnings = TRUE) {
  pkgName <- dcf[["Package"]]
  #   pkgRow <- match(pkgName, rownames(pdb))
  pkgRow <- match(pkgName, pdb[, "Package"])
  newRow <- with(dcf,
                 c(Package, Version, NA,
                   Depends, Imports, LinkingTo, Suggests, Enhances, License,
                   rep(NA, 8)))
  if (!is.na(pkgRow)) {
    pdb[pkgRow, ] <- newRow
    if (warnings) {
      warning(sprintf("Overwriting package information for: %s", pkgName))
    }
  } else {
    pdb <- rbind(pdb, newRow)
    rownames(pdb)[nrow(pdb)] <- pkgName
  }
  pdb
}



# from http://stackoverflow.com/questions/13163248 Possible to override the
# blocking of a package's (re-)installation after it has been required/loaded?

#' @importFrom httr GET stop_for_status content
readDescriptionGithub <- function(repo, username, branch = "master", quiet = TRUE) {
  if (!missing(username) && !is.null(username)) repo <- paste(username, repo, sep = "/")
  pkg <- sprintf("https://github.com/%s/raw/%s/DESCRIPTION", repo, branch)
  ff <- tempfile()
  on.exit(unlink(ff))
  #   download.file(url=pkg, destfile=ff, quiet=quiet, method="curl")
  request <- GET(pkg)
  stop_for_status(request)
  writeBin(content(request), ff)

  readDescription(ff)
}



#' Add DESCRIPTION information from package on github.
#'
#' Downloads the DESCRIPTION file from a package on github, parses the fields
#' and adds (or replaces) a row in the available package database.
#'
#' @param pdb Package database, usually the result of [pkgAvail()] or
#'   [available.packages()]
#' @param repo Character vector. Name of repository on github, e.g.
#'   `"andrie/rrd"`
#' @param username Optional character vector. Name of repository on github, e.g.
#'   `"andrie/rrd"`
#' @param branch name of branch, defaults to `"master"`

#' @export
#' @family github functions
#'
#' @example /inst/examples/example_addPackageListingGithub.R
addPackageListingGithub <- function(
  pdb = pkgAvail(), repo, username = NULL, branch = "master"
) {
  desc <- readDescriptionGithub(repo = repo, username = username, branch = branch)
  addPackageListing(pdb, desc)
}
