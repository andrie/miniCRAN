# This code is from package pkgDepTools
# http://www.bioconductor.org/packages/release/bioc/html/pkgDepTools.html
# Author: Seth Falcom

basicInstallOrder <- function(pkg, depG) {
  ## Helper function to return the complete install order
  ## for the given package.
  allPkgs <- c(pkg, names(acc(depG, pkg)[[1]]))
  if (length(allPkgs) > 1) {
    pkgSub <- subGraph(allPkgs, depG)
    toInst <- tsort(pkgSub) 
    if (!is.character(toInst))
      stop("depG is not a DAG")
    rev(toInst)
  } else {
    allPkgs
  }
}

# Copy of tools:::split_op_version
split_op_version <- function (x) {
  pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
  x1 <- sub(pat, "\\1", x)
  x2 <- sub(pat, "\\2", x)
  if (x2 != x1) {
    pat <- "[[:space:]]*([[<>=!]+)[[:space:]]+(.*)"
    version <- sub(pat, "\\2", x2)
    if (!grepl("^r", version)) 
      version <- package_version(version)
    list(name = x1, op = sub(pat, "\\1", x2), version = version)
  }
  else list(name = x1)
}

# Copy of tools:::.split_dependencies
split_dependencies <- function (x) {
  if (!length(x)) 
    return(list())
  x <- unlist(strsplit(x, ","))
  x <- sub("[[:space:]]+$", "", x)
  x <- unique(sub("^[[:space:]]*(.*)", "\\1", x))
  names(x) <- sub("^([[:alnum:].]+).*$", "\\1", x)
  lapply(x, split_op_version)
}

cleanPkgField <- function(val) {
  ## Given the value from a field like 'Depends' in a package's
  ## DESCRIPTION file, return a character vector of package names
  ## with the version restrictions stripped and \R~removed.
  ## FIXME: uses a private function from tools
  if (is.na(val))
    return(character(0))
  val <- names(split_dependencies(val))
  if (is.null(val))
    return(character(0))
  val <- val[! val %in% "R"]
  if (length(val))
    return(val)
  return(character(0))
}

getDownloadSize <- function(url) {
  if (globals$have_RCurl) {
    h <- basicTextGatherer()
    junk <- getURI(url, writeheader=h$update, header=TRUE, nobody=TRUE)
    h <- h$value()
    ans <- parseContentLength(h)
  } else {
    ans <- as.numeric(NA)
  }
  ans
}

getDownloadSizes <- function(urls) {
  ## Fetch HTTP headers for a vector of URLs
  ## This appears to be much faster than looping over getDownloadSize
  ## for individual URLs.
  if (globals$have_RCurl) {
    h <- multiTextGatherer(urls)
    junk <- getURIAsynchronous(urls, write=h, header=TRUE, nobody=TRUE)
    headerContents <- sapply(h, function(x) {
      parseContentLength(x$value())
    })
  } else {
    headerContents <- rep(as.numeric(NA), length(urls))
    names(headerContents) <- urls
  }
  headerContents
}


getDownloadSizesBatched <- function(urls) {
  if (globals$have_RCurl) {
    BATCH <- 20
    done <- FALSE
    start <- 1
    N <- length(urls)
    headerContents <- numeric(N)
    while(!done) {
      end <- start + BATCH
      if (end >= N) {
        end <- N
        done <- TRUE
      }
      batchIdx <- seq.int(start, end)
      h <- multiTextGatherer(urls[batchIdx])
      junk <- getURIAsynchronous(urls[batchIdx], write=h,
                                 header=TRUE, nobody=TRUE)
      headerContents[batchIdx] <- sapply(h, function(x) {
        parseContentLength(x$value())
      })
      start <- end + 1
    }
  } else {
    headerContents <- rep(as.numeric(NA), length(urls))
  }
  names(headerContents) <- urls
  headerContents
}


getInstallOrder <- function(pkg, depG, needed.only=TRUE) {
  toInst <- basicInstallOrder(pkg, depG)
  if (needed.only)
    toInst <- setdiff(toInst, rownames(installed.packages()))
  if (length(toInst)) {
    sizes <- unlist(nodeData(depG, n=toInst, attr="size"))
    missingSize <- is.na(sizes)
    if (!all(missingSize)) {
      sizesC <- as.character(round(sizes, 2))
      sizesC[is.na(sizes)] <- "?"
      sizesC <- paste(sizesC, "MB", sep="")
      names(toInst) <- sizesC
      total <- sum(sizes[!is.na(sizes)])
    } else {
      total <- as.integer(NA)
    }
    list(packages=toInst, total.size=total)
  } else {
    list(packages=character(0), total.size=numeric(0))
  }
}


makePkgUrl <- function(pMat, type=getOption("pkgType")) {
  pkg <- paste(pMat[, "Package"], pMat[, "Version"], sep="_")
  ext <- switch(type,
                source="tar.gz",
                win.binary="zip",
                mac.binary="tgz")
  pkg <- paste(pkg, ext, sep=".")
  paste(pMat[, "Repository"], pkg, sep="/")
}


parseContentLength <- function(h) {
  fldName <- "Content-Length:"
  sizeRegex <- "^.*Content-Length: ([0-9]+)[\r\n]+.*"
  ## Return size in megabytes
  if (length(grep(fldName, h)))
    as.numeric(sub(sizeRegex, "\\1", h)) / 1024^2
  else
    as.numeric(NA)
}
