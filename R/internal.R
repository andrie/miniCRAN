#' Get the path to the repo directory containing the package files.
#'
#' @note Not all versions of R are compatible with with all package types (e.g.,
#'   `mac.binary.el-capitan` is only valid for R > 3.4.0).
#'
#' @template Rversion
#'
#' @param type  character, indicating the type of package to download and
#'   install. See [install.packages()].
#'
#' @template repo_folder_structure
#'
#' @return The filepath to the package files directory.
#'
#' @keywords Internal
#'   
repoPrefix <- function(type, Rversion) {
  Rversion <- twodigitRversion(Rversion)
  
  if ((type == "mac.binary.el-capitan") && (numeric_version(Rversion) < "3.4")) {
    warning("Type mac.binary.el-capitan only valid for R >= 3.4")
  } else if ((type == "mac.binary.mavericks") && (numeric_version(Rversion) >= "3.4")) {
    warning("Type mac.binary.mavericks only valid for R < 3.4")
  }
  
  switch(
    type,
    "source" = "src/contrib",
    "win.binary" = sprintf("bin/windows/contrib/%s", Rversion),
    "mac.binary" = sprintf("bin/macosx/contrib/%s", Rversion),
    "mac.binary.el-capitan" = sprintf("bin/macosx/el-capitan/contrib/%s", Rversion),
    "mac.binary.leopard" = sprintf("bin/macosx/leopard/contrib/%s", Rversion),
    "mac.binary.mavericks" =  sprintf("bin/macosx/mavericks/contrib/%s", Rversion),
    stop("Type ", type, " not recognised.")
  )
}

#' Construct path to full binary location
#' @inheritParams makeRepo
#' @inheritParams repoPrefix
#' 
#' @keywords Internal
repoBinPath <- function(path, type, Rversion) {
  normalizePath(file.path(path, repoPrefix(type, Rversion)), mustWork = FALSE, winslash = "/")
}

#' Get a two-digit version of the R version
#'
#' @template Rversion
#'
#' @return A character string representing the two-digit R version.
#'
#' @importFrom methods is
#' 
#' @keywords Internal
#'
twodigitRversion <- function(Rversion = R.version) {
  if ("simple.list" %in% is(Rversion)) {
    paste(Rversion$major, strsplit(Rversion$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
  } else if ("R_system_version" %in% is(Rversion)) {
    paste(strsplit(as.character(Rversion), ".", fixed = TRUE)[[1L]][1L:2L], collapse = ".")
  } else if (is.character(Rversion)) {
    paste(strsplit(Rversion, ".", fixed = TRUE)[[1L]][1L:2L], collapse = ".")
  } else if (is.list(Rversion)) {
    paste(Rversion$major, Rversion$minor, sep = ".")
  } else {
    Rversion
  }
}
  