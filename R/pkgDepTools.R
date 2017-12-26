# Code copied from the pkgDepTools project
# Copyright (C) Seth Falcon
# http://www.bioconductor.org/packages/release/bioc/html/pkgDepTools.html
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License version 2
# as published by the Free Software Foundation



# Code copied from the pkgDepTools project
# Copyright (C) Seth Falcon
# http://www.bioconductor.org/packages/release/bioc/html/pkgDepTools.html


# Copy of tools:::split_op_version.

# @rdname pkgDepTools
# @keywords internal
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


# Copy of tools:::.split_dependencies.

# @rdname pkgDepTools
# @keywords internal
split_dependencies <- function(x) {
  if (!length(x)) return(list())
  x <- unlist(strsplit(x, ","))
  x <- sub("[[:space:]]+$", "", x)
  x <- unique(sub("^[[:space:]]*(.*)", "\\1", x))
  names(x) <- sub("^([[:alnum:].]+).*$", "\\1", x)
  lapply(x, split_op_version)
}


# Clean package fields.
# 
# Given the value from a field like 'Depends' in a package's DESCRIPTION file, return a character vector of package names with the version restrictions stripped and \R~removed.
# @param val Value from a field like 'Depends' in a package's DESCRIPTION file
# @rdname pkgDepTools
# @keywords internal
cleanPkgField <- function(val) {
  if (is.na(val)) return(character(0))
  val <- names(split_dependencies(val))
  if (is.null(val)) return(character(0))
  val <- val[!val %in% "R"]
  if (length(val)) return(val)
  character(0)
}
