################################################################################
#' Compare miniCRAN Packages with CRAN-like Repositories
#'
#' \code{oldPackages} indicates packages which have a (suitable) later
#' version on the repositories whereas \code{\link{updatePackages}} offers to
#' download and install such packages.
#'
#' These functions are based on \code{\link{update.packages}} and related,
#' except rather than looking for locally installed packages they look for the
#' package sources and binaries being hosted in the miniCRAN repository.
#'
#' @param path  The local path to the directory where the miniCRAN repo resides.
#'
#' @param repos character vector, the base URL(s) of the repositories to use,
#' e.g., the URL of a CRAN mirror such as "\code{http://cran.us.r-project.org}".
#'
#' @param contriburl  URL(s) of the contrib sections of the repositories.
#' Use this argument if your repository is incomplete. Overrides argument repos.
#'
#' @param method  Download method, see \code{\link{download.file}}.
#'
#' @param available an object as returned by \code{\link{available.packages}}
#' listing packages available at the repositories, or \code{NULL} which makes an
#' internal call to \code{\link{available.packages}}.
#'
#' @param availPkgs by default all packages hosted in the miniCRAN repo,
#' \code{\link{pkgAvail}(repos=path, type=type)}. A subset can be specified;
#' currently this must be in the same (character matrix) format as returned by
#' \code{pkgAvail()}.
#'
#' @param type  character, indicating the type of package to download and
#'  install. See \code{\link{install.packages}}.
#'
#' @param Rversion numeric version of the R system for which to fetch packages.
#' See \code{\link{R_system_version}}.
#'
#' @return \code{NULL} or a matrix with one row per package, row names the
#' package names and column names "Package", "LocalVer", "ReposVer", and
#' "Repository".
#'
#' @seealso \code{\link{updatePackages}}, \code{\link{pkgAvail}}.
#' @family miniCRAN functions
#'
#' @export
#' @docType methods
#'
#' @example /inst/examples/example_updatePackages.R
#'
oldPackages <- function (path=NULL, repos=getOption("repos"),
                         contriburl=contrib.url(repos, type),
                         availPkgs=pkgAvail(repos=path, type=type),
                         method, available=NULL, type="source",
                         Rversion=getRversion()) {
  if (is.null(path)) stop("path to miniCRAN repo must be specified")
  if (!missing(availPkgs)) {
    if (!is.matrix(availPkgs) || !is.character(availPkgs[, "Package"]))
      stop("ill-formed 'availPkgs' matrix")
  }
  if (NROW(availPkgs) == 0L) return(NULL)

  if (is.null(available)) {
    available <- available.packages(contriburl=contriburl, method=method)
  }
  update <- NULL
  currentR <- minorR <- Rversion
  minorR[[c(1L, 3L)]] <- 0L
  for (k in 1L:nrow(availPkgs)) {
    if (availPkgs[k, "Priority"] %in% "base")
      next
    z <- match(availPkgs[k, "Package"], available[, "Package"])
    if (is.na(z))
      next
    onRepos <- available[z, ]
    if (package_version(onRepos["Version"]) <= package_version(availPkgs[k, "Version"]))
      next
    deps <- onRepos["Depends"]
    if (!is.na(deps)) {
      Rdeps <- split_dependencies(deps)[["R", exact = TRUE]]
      if (length(Rdeps) > 1L) {
        target <- Rdeps$version
        res <- do.call(Rdeps$op, list(currentR, target))
        if (!res)
          next
      }
    }
    update <- rbind(update, c(availPkgs[k, c("Package", "Version")],
                              onRepos["Version"], onRepos["Repository"]))
  }
  if (!is.null(update)) {
    colnames(update) <- c("Package", "LocalVer", "ReposVer", "Repository")
  }
  rownames(update) <- update[, "Package"]
  update[!duplicated(update), , drop = FALSE]
}

################################################################################
#' Compare miniCRAN Packages with CRAN-like Repositories
#'
#' \code{\link{oldPackages}} indicates packages which have a (suitable) later
#' version on the repositories whereas \code{updatePackages} offers to
#' download and install such packages.
#'
#' These functions are based on \code{\link{update.packages}} and related,
#' except rather than looking for locally installed packages they look for the
#' package sources and binaries being hosted in the miniCRAN repository.
#'
#' @inheritParams oldPackages
#'
#' @param oldPkgs if specified as non-NULL, updatePackages() only considers
#' these packages for updating. This may be a character vector of package names
#' or a matrix as returned by oldPackages().
#'
#' @param ask logical indicating whether to ask user before packages are
#' actually downloaded and installed, or the character string "\code{graphics}",
#' which brings up a widget to allow the user to (de-)select from the list of
#' packages which could be updated or added. The latter value only works on
#' systems with a GUI version of \code{\link{select.list}}, and is otherwise
#' equivalent to \code{ask = TRUE}.
#'
#' @return \code{NULL} invisibly.
#'
#' @seealso \code{\link{oldPackages}}
#' @family miniCRAN functions
#' @docType methods
#' @export
#' @example /inst/examples/example_updatePackages.R
#'
updatePackages <- function (path=NULL, repos=getOption("repos"),
                            contriburl=contrib.url(repos, type),
                            method, ask=TRUE, available=NULL,
                            oldPkgs=NULL, type="source",
                            Rversion=getRversion()) {
  force(ask)
  simplifyRepos <- function(repos, type) {
    tail <- substring(contrib.url("---", type), 4)
    ind <- regexpr(tail, repos, fixed=TRUE)
    ind <- ifelse(ind > 0, ind-1, nchar(repos, type="c"))
    substr(repos, 1, ind)
  }
  text.select <- function(old) {
    update <- NULL
    for (k in seq_len(nrow(old))) {
      cat(old[k, "Package"], ":\n",
          "Local Version", old[k, "LocalVer"], "\n",
          "Repos Version", old[k, "ReposVer"],
          "available at", simplifyRepos(old[k, "Repository"], type))
      cat("\n")
      answer <- substr(readline("Update (y/N/c)?  "), 1L, 1L)
      if (answer == "c" | answer == "C") {
        cat("cancelled by user\n")
        return(invisible())
      }
      if (answer == "y" | answer == "Y") update <- rbind(update, old[k, ])
    }
    update
  }
  if (is.null(path)) stop("path to miniCRAN repo must be specified")
  if (is.null(available)) {
    available <- available.packages(contriburl=contriburl, method=method)
  }
  if (!is.matrix(oldPkgs) && is.character(oldPkgs)) {
    subset <- oldPkgs
    oldPkgs <- NULL
  } else {
    subset <- NULL
  }
  if (is.null(oldPkgs)) {
    oldPkgs <- oldPackages(path=path, repos=repos, contriburl=contriburl,
                           method=method, available=available, type=type,
                           Rversion=Rversion)
    if (is.null(oldPkgs)) {
      message("All packages are up to date from repos: ", names(repos))
      return(invisible())
    }
  } else if (!(is.matrix(oldPkgs) && is.character(oldPkgs))) {
    stop("invalid 'oldPkgs'; must be a character vector or a result from oldPackages()")
  }
  if (!is.null(subset)) {
    oldPkgs <- oldPkgs[rownames(oldPkgs) %in% subset, , drop = FALSE]
    if (nrow(oldPkgs)==0) return(invisible())
  }
  update <- if (is.character(ask) && ask == "graphics") {
    if (.Platform$OS.type=="windows" || .Platform$GUI ==
          "AQUA" || (capabilities("tcltk") && capabilities("X11"))) {
      k <- select.list(oldPkgs[, 1L], oldPkgs[, 1L], multiple=TRUE,
                       title="Packages to be updated", graphics = TRUE)
      oldPkgs[match(k, oldPkgs[, 1L]), , drop = FALSE]
    } else {
      text.select(oldPkgs)
    }
  } else if (isTRUE(ask)) {
    text.select(oldPkgs)
  } else {
    oldPkgs
  }
  if (length(update[,"Package"])) {
    addPackage(update[,"Package"], path=path, repos=repos, type=type)
  }
}
