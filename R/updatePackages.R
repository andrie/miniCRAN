#' Check for available package updates in a miniCRAN repo.
#'
#' `oldPackages()` indicates packages which have a (suitable) later version on
#' the repositories whereas `updatePackages()` offers to download and install
#' such packages.
#'
#' These functions are based on [update.packages()].  However, rather than
#' looking for locally installed packages they look for the package source and
#' binaries in the miniCRAN repository.
#'
#' @name updatePackages
#'
#' @inheritParams makeRepo
#' @inheritParams pkgDep
#'
#' @param method  Download method, see [download.file()].
#'
#' @param availableLocal all packages hosted in the miniCRAN repo, as returned
#'   by [pkgAvail()]. A subset can be specified; currently this must be in the
#'   same (character matrix) format as returned by [pkgAvail()].
#'
#' @return `oldPackages()` returns a matrix with one row per package and columns
#'   for "Package", "LocalVer", "ReposVer" and "Repository".  The matrix row
#'   names the package names.
#'
#' @seealso [updatePackages()], [pkgAvail()].
#'
#' @export
#' @family update repo functions
#'
#' @example /inst/examples/example_updatePackages.R
#'
oldPackages <- function(
  path = NULL,
  repos = getOption("repos"),
  availPkgs = pkgAvail(repos = repos, type = type, Rversion = Rversion),
  method,
  availableLocal = pkgAvail(
    repos = path,
    type = type,
    Rversion = Rversion,
    quiet = quiet
  ),
  type = "source",
  Rversion = R.version,
  quiet = FALSE
) {
  if (is.null(path)) stop("path to miniCRAN repo must be specified")
  if (!missing(availPkgs)) {
    if (!is.matrix(availPkgs) || !is.character(availPkgs[, "Package"]))
      stop("ill-formed 'availPkgs' matrix")
  }
  if (NROW(availPkgs) == 0L) stop("Invalid remote repository")
  if (NROW(availableLocal) == 0L) stop("Invalid local repository")

  idx <- match(availableLocal[, "Package"], availPkgs[, "Package"])
  compare <- sapply(seq_along(idx), function(i) {
    compareVersion(
      (availPkgs[idx[i], "Version"]),
      (availableLocal[i, "Version"])
    ) >
      0
  })

  update <- cbind(
    availableLocal[compare, c("Package", "Version"), drop = FALSE],
    availPkgs[idx[compare], c("Version", "Repository"), drop = FALSE]
  )
  colnames(update) <- c("Package", "LocalVer", "ReposVer", "Repository")
  update
}


simplifyRepos <- function(repos, t, Rversion) {
  tail <- substring(contribUrl("---", type = t, Rversion = Rversion), 4)
  ind <- regexpr(tail, repos, fixed = TRUE)
  ind <- ifelse(ind > 0, ind - 1, nchar(repos, type = "c"))
  substr(repos, 1, ind)
}

graphics_capable <- function() {
  .Platform$OS.type == "windows" ||
    .Platform$GUI == "AQUA" ||
    (capabilities("tcltk") && capabilities("X11"))
}

select_from_list <- function(choices, preselect, multiple, title, graphics) {
  utils::select.list(choices, preselect, multiple, title, graphics)
}

ask_to_update <- function(oldPkgs, t, Rversion, ask = FALSE) {
  z <-
    if (is.character(ask) && ask == "graphics") {
      if (graphics_capable()) {
        k <- select_from_list(
          choices = oldPkgs[, 1L],
          preselect = oldPkgs[, 1L],
          multiple = TRUE,
          title = "Packages to be updated",
          graphics = TRUE
        )
        oldPkgs[match(k, oldPkgs[, 1L]), , drop = FALSE]
      } else {
        ask_to_update_package(oldPkgs, t, Rversion)
      }
    } else {
      if (isTRUE(ask)) {
        ask_to_update_package(oldPkgs, t, Rversion)
      } else {
        oldPkgs
      }
    }
  rownames(z) <- unname(z[, "Package"])
  z
}

read_line_wrapper <- function(prompt = "") {
  base::readline(prompt)
}

ask_to_update_package <- function(old, t, Rversion) {
  update <- NULL
  for (k in seq_len(nrow(old))) {
    cat(
      old[k, "Package"],
      ":\n",
      "Local Version",
      old[k, "LocalVer"],
      "\n",
      "Repos Version",
      old[k, "ReposVer"],
      "available at",
      simplifyRepos(old[k, "Repository"], t, Rversion)
    )
    cat("\n")
    answer <- tolower(substr(read_line_wrapper("Update (y/N/c)?  "), 1L, 1L))
    if (answer == "c") {
      cat("cancelled by user\n")
      return(invisible())
    }
    if (answer == "y") {
      update <- rbind(update, old[k, ])
    }
  }
  update
}


#' @inheritParams makeRepo
#'
#' @param oldPkgs if specified as non-NULL, `updatePackages()` only considers
#'   these packages for updating. This may be a character vector of package
#'   names or a matrix as returned by `oldPackages()`.
#'
#' @param ask logical indicating whether to ask user before packages are
#'   actually downloaded and installed.  Alternatively, the value `"graphics"`
#'   starts an interactive widget to allow the user to (de-)select from the list of
#'   packages which could be updated or added. The latter value only works on
#'   systems with a GUI version of [select.list()], and is otherwise equivalent
#'   to `ask = TRUE`.
#'
#' @return `updatePackages` returns `NULL` invisibly.
#'
#' @export
#'
updatePackages <- function(
  path = NULL,
  repos = getOption("repos"),
  method = NULL,
  ask = TRUE,
  availPkgs = pkgAvail(repos = repos, type = type, Rversion = Rversion),
  oldPkgs = NULL,
  type = "source",
  Rversion = R.version,
  quiet = FALSE
) {
  assert_that(is_path(path))

  do_one <- function(t, Rversion, ask) {
    if (!is.matrix(oldPkgs) && is.character(oldPkgs)) {
      subset <- oldPkgs
      oldPkgs <- NULL
    } else {
      subset <- NULL
    }
    if (is.null(oldPkgs)) {
      oldPkgs <- oldPackages(
        path = path,
        repos = repos,
        method = method,
        availPkgs = availPkgs,
        type = t,
        Rversion = Rversion
      )
      if (is.null(oldPkgs)) {
        message("All packages are up to date from repos: ", names(repos))
        return(invisible())
      }
    } else {
      if (!(is.matrix(oldPkgs) && is.character(oldPkgs))) {
        msg <- paste0(
          "invalid 'oldPkgs'; ",
          "must be a character vector or a result from oldPackages()"
        )
        stop(msg)
      }
    }
    if (!is.null(subset)) {
      oldPkgs <- oldPkgs[rownames(oldPkgs) %in% subset, , drop = FALSE]
      if (nrow(oldPkgs) == 0) return(invisible())
    }

    update <- ask_to_update(
      oldPkgs = oldPkgs,
      t = t,
      Rversion = Rversion,
      ask = ask
    )

    if (length(update[, "Package"])) {
      addPackage(
        update[, "Package"],
        path = path,
        repos = repos,
        type = t,
        quiet = quiet,
        deps = FALSE,
        Rversion = Rversion
      )
    }
  }

  lapply(type, do_one, Rversion = Rversion, ask = ask)
}
