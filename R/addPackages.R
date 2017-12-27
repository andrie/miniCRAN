if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("."))
}

#' Check for previous versions of packages in a miniCRAN repository.
#'
#' Checks for previous versions, and returns the file paths for packages with multiple versions. You can subsequently decide which version to keep.
#'
#' @param pkgs Character vector of packages to be installed. If not provided, checks all files for multiple package versions.
#'
#' @param path The local path to the directory where the miniCRAN repo resides.
#'
#' @param type  character, indicating the type of package to download and install. See [install.packages()].
#'
#' @param Rversion Version of R. Can be specified as a character string with the two digit R version, e.g. "3.1".  Defaults to [R.version]
#'
#' @return Returns invisibly the filepaths to packages with multiple versions for removal.
#'
#' @export
#' @family update repo functions
#'
#' @example /inst/examples/example_checkVersions.R
#'
checkVersions <- function(pkgs = NULL, path = NULL, type = "source",
                          Rversion = R.version) {
  if (is.null(path)) stop("path must be specified.")
  if (!file.exists(path)) stop("invalid path, ", path)

  do_one <- function(type) {
    pkgPath <- repoBinPath(path, type, Rversion)
    if (is.null(pkgs)) {
      files <- dir(pkgPath)
    } else {
      files <- sapply(pkgs, function(x) list.files(pkgPath, pattern = paste0(x, "_")) )
    }
    files <- unlist(files)
    pkgFiles <- grep("\\.(tar\\.gz|zip|tgz)$", basename(files), value = TRUE)
    
    # identify duplicate packages and warn the user
    pkgs <- sapply(strsplit(files, "_"), "[[", 1)
    dupes <- pkgs[duplicated(pkgs)]
    if (length(dupes)) warning("Duplicate package(s): ", paste(dupes, collapse = ", "))
    file.path(pkgPath, pkgFiles)
  }
  
  duplicatePkgs <- sapply(type, do_one, simplify = FALSE)
  names(duplicatePkgs) <- type
  duplicatePkgs
}


#  ------------------------------------------------------------------------


#' Add packages to a miniCRAN repository.
#'
#' @inheritParams makeRepo
#' @inheritParams pkgDep
#'
#' @param Rversion Version of R. Can be specified as a character string with the two digit R version, e.g. "3.1".  Defaults to [R.version]
#'
#' @param deps logical indicating whether the package dependencies should be added (default `TRUE`).
#'
#' @return Installs the packages, rebuilds the package index, and invisibly returns the number of packages written to the index files.
#'
#' @importFrom tools write_PACKAGES
#' @export
#' @family update repo functions
#'
#' @example /inst/examples/example_checkVersions.R
#'
addPackage <- function(pkgs = NULL, path = NULL, repos = getOption("repos"),
                       type = "source", Rversion = R.version,
                       writePACKAGES = TRUE, deps = TRUE, quiet = FALSE) {
  if (is.null(path) || is.null(pkgs)) stop("path and pkgs must both be specified.")

  do_one <- function(t) {
    prev <- checkVersions(pkgs = pkgs, path = path, type = t, Rversion = Rversion)
    prev <- prev[[1]]
    prev.df <- getPkgVersFromFile(prev)
    
    if (deps) pkgs <- pkgDep(pkgs, repos = repos, type = t, Rversion = Rversion)
    
    makeRepo(pkgs = pkgs, path = path, repos = repos, type = t, Rversion = Rversion,
             download = TRUE, writePACKAGES = FALSE, quiet = quiet)
    
    if (length(prev)) {
      curr <- suppressWarnings(
        checkVersions(pkgs = pkgs, path = path, type = t, Rversion = Rversion)
      )
      curr <- curr[[1]]
      curr.df <- getPkgVersFromFile(curr)
        
      findPrevPackage <- function(x) {
        grep(paste0("^", x), basename(prev)) 
        }
      
      dupes <- with(curr.df, package[duplicated(package)])
      if (length(dupes)) {
        to_remove <- lapply(dupes, findPrevPackage)
        if (length(unlist(to_remove))) {
          file.remove(prev[unlist(to_remove)])
        }
      }
    }
  }

  lapply(type, do_one)

  n <- if (writePACKAGES) updateRepoIndex(path = path, type = type, Rversion = Rversion)
  invisible(n)
}



#  ------------------------------------------------------------------------


#' Add old package versions to a miniCRAN repository.
#'
#' Will download and add older source package versions. Older binary versions are not normally available on CRAN and should be build from source on the platform for which they are required. As such, specifying `type!="source"` will likely fail as the download will not be successful.
#'
#' @inheritParams addPackage
#' @inheritParams makeRepo
#'
#' @param vers The package version to install.
#'
#' @return Adds the packages, rebuilds the package index, and invisibly returns the number of packages written to the index files.
#'
#' @note Dependencies for old package versions cannot be determined automatically and must be specified by the user in `pkgs` and `vers`. Thus, `deps=FALSE` is the default for this function.
#'
#' @importFrom tools write_PACKAGES
#' @export
#' @family update repo functions
#'
#' @example /inst/examples/example_checkVersions.R
#'
addOldPackage <- function(pkgs = NULL, path = NULL, vers = NULL,
                          repos = getOption("repos"),
                          type = "source", Rversion = R.version,
                          writePACKAGES=TRUE, deps=FALSE, quiet=TRUE) {
  if (is.null(path) || is.null(pkgs) || is.null(vers)) {
    stop("path, pkgs, and vers must all be specified.")
  }
  if (type != "source") stop("Older binary versions are not normally available on CRAN. ",
                             "You must build the binary versions from source.")
  if (deps) {
    message("Unable to automatically determine dependency version information.\n",
            "Use pkgs and vers to identify which dependecies and their versions to download.")
  }
  vers <- as.character(vers)
  oldPkgs <- file.path(repos, repoPrefix(type, R.version), "Archive",
                       pkgs, sprintf("%s_%s%s", pkgs, vers, pkgFileExt(type)))

  pkgPath <- repoBinPath(path = path, type = type, Rversion = Rversion)
  if (!file.exists(pkgPath)) dir.create(pkgPath, recursive = TRUE)
  
  do_one <- function(x) {
    result <- download.file(x, destfile = file.path(pkgPath, basename(x)),
                                   method = "auto", mode = "wb", quiet = quiet)
    if (result != 0) warning("error downloading file ", x)
  }
  sapply(oldPkgs, do_one)
  if (writePACKAGES) invisible(updateRepoIndex(path = path, type = type, Rversion))
}


# ------------------------------------------------------------------------------


#' List pre-built packages in a directory based on file extension
#'
#' @param pkgs  Character vector of package names
#' @param path  Character string specifying the directory containing packages to be added.
#' @param type  Character indicating the package type (e.g., "source", "win.binary", etc.).
#'
#' @return Installs the packages and returns the new package index.
#'
#' @rdname listFiles
#' @docType methods
#'
#' @examples
#' \dontrun{
#'  .listFiles('path/to/my/packages', type = "source")
#' }
#'
.listFiles <- function(pkgs, path, type) {
  stopifnot(dir.exists(path))
  pattern <- pkgFileExt(type)

  # get a list of all files in pkgPaths directory matching pattern
  f <- list.files(path, pattern = pattern)

  # we only care about the subset matching pkgs
  f <- sapply(pkgs, function(x) { grep(x, f, value = TRUE) })

  if (length(f)) {
    # if multiple versions present, always use latest
    fp <- local({
      x <- strsplit(f, "_")
      sapply(x, `[[`, 1)
    })
    
    fv <- local({
      x <- strsplit(f, "_")
      x <- sapply(x, `[[`, 2)
      x <- strsplit(x, pattern)
      x <- sapply(x, `[[`, 1)
      as.numeric_version(x)
    })
    
    fout <- sapply(fp, function(x) {
      ids.p <- which(fp %in% x)

      # numeric_version always returns version using '.' as separator,
      #   even if the package uses '-', so we need to ensure either will work
      id.v <- which(fv == max(fv[ids.p]))

      f[id.v]
    })
    unique(fout)
  } else {
    character()
  }
}



#' Add local packages to a miniCRAN repository.
#'
#' Examine the contents of a directory specified by `pkgPath` for pre-built packages matching the names specified by `pkgs`, and add these to the miniCRAN repository.
#'
#' To build a package from source and then add it, use `build = TRUE`. Note that package development libraries and the `devtools` package must be installed on your system in order to build packages.
#'
#' @note Currently, adding local packages does not check nor download their dependencies.
#'
#' @inheritParams addPackage
#' @param pkgPath  Character vector of directory location containing packages to be added. Note that `pkgPath` should be the parent directory of the package (i.e., the package directory path is constructed from `file.path(pkgPath, pkgs)`).
#' @param build    Logical indicating whether packages should be build prior to adding.
#'
#' @return Installs the packages and returns the new package index.
#'
#' @export
#' @docType methods
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'  addLocalPackage("myPackage", "path/to/my/prebuilt/package",
#'                  "path/to/my/miniCRAN/repo")
#'
#'  addLocalPackage("myPackage", "path/to/my/package/sourcecode",
#'                  "path/to/my/miniCRAN/repo", build = TRUE)
#' }
#'
addLocalPackage <- function(pkgs = NULL, pkgPath = NULL, path = NULL,
                            type = "source", Rversion = R.version,
                            writePACKAGES = TRUE, deps = FALSE,
                            quiet = FALSE, build = FALSE) {
  if (is.null(path) || is.null(pkgs) || is.null(pkgPath)) {
    stop("path, pkgs, and pkgPath must be specified.")
  }

  stopifnot(dir.exists(file.path(pkgPath)))

  # build local package if needed
  if (isTRUE(build)) {
    warning("Building local packages is still being tested.")
    if (requireNamespace("devtools", quietly = TRUE)) {
      lapply(pkgs, function(x) {
        devtools::build(pkg = file.path(pkgPath, x), path = pkgPath,
                        binary = ifelse(type == "source", FALSE, TRUE),
                        quiet = quiet)
      })
    } else {
      stop("To build packages, you must first install the 'devtools' package.")
    }
  }

  # get list of pre-built packages for each type, filter by pkgs to be added
  sapply(type, function(t) {
    repoPath <- file.path(path, repoPrefix(t, Rversion))
    if (!dir.exists(repoPath)) dir.create(repoPath, recursive = TRUE)
    files <- .listFiles(pkgs = pkgs, path = pkgPath, type = t)

    # check for previous package version and omit if identical
    prev <- checkVersions(pkgs, path)
    same <- which(basename(as.character(prev)) %in% files)

    if (length(same)) {
      files <- files[-same]
      if (length(files) == 0) {
        if (!quiet) message("All packages up to date. Nothing to add.")
        return(invisible(NULL))
      }
    }

    # copy other packages to their respective folders
    lapply(files, function(x) {
      f.src <- file.path(pkgPath, x)
      f.dst <- file.path(repoPath, x)

      file.exists(f.src)

      if (!isTRUE(quiet)) message("copying ", x, "\n")
      file.copy(from = f.src, to = f.dst)
    })

    # check to ensure they all copied successfully
    copied <- file.exists(file.path(repoPath, files))
    if (!all(copied)) {
      warning("the following ", t, " packages were not copied:\n",
              paste(files[!copied], sep = ", "))
    }

    # remove previous package versions
    if (length(prev[-same]) > 0) unlink(prev[-same])
  })

  # write package index for each folder:
  index <- updateRepoIndex(path = path, type = type, Rversion = Rversion)

  invisible(index)
}
