# Add packages to a miniCRAN repository.

Add packages to a miniCRAN repository.

## Usage

``` r
addPackage(
  pkgs = NULL,
  path = NULL,
  repos = getOption("repos"),
  type = "source",
  Rversion = R.version,
  writePACKAGES = TRUE,
  deps = TRUE,
  quiet = FALSE
)
```

## Arguments

- pkgs:

  Character vector of packages to download

- path:

  Destination download path. This path is the root folder of your new
  repository.

- repos:

  URL(s) of the 'contrib' sections of the repositories, e.g.
  `"https://cran.us.r-project.org"`. Passed to
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- type:

  Possible values are (currently) "source", "mac.binary" and
  "win.binary": the binary types can be listed and downloaded but not
  installed on other platforms. Passed to
  [`download.packages()`](https://rdrr.io/r/utils/download.packages.html).

- Rversion:

  Version of R (only used if `type` is not `source`.) Defaults to
  [R.version](https://rdrr.io/r/base/Version.html), but this can be
  specified as any of the following formats:

  - a character string with the two digit R version, e.g. "3.1"

  - a list with components `major` and `minor`

  - the result of
    [`getRversion()`](https://rdrr.io/r/base/numeric_version.html)

  - the result of [R.version](https://rdrr.io/r/base/Version.html)

- writePACKAGES:

  If TRUE, calls `write_PACKAGES()` to update the repository PACKAGES
  file.

- deps:

  logical indicating whether the package dependencies should be added
  (default `TRUE`).

- quiet:

  If TRUE, suppress status messages (if any), and the progress bar
  during download.

## Value

Installs the packages, rebuilds the package index, and invisibly returns
the number of packages written to the index files.

## See also

Other update repo functions:
[`addOldPackage()`](https://andrie.github.io/miniCRAN/reference/addOldPackage.md),
[`checkVersions()`](https://andrie.github.io/miniCRAN/reference/checkVersions.md),
[`makeRepo()`](https://andrie.github.io/miniCRAN/reference/makeRepo.md),
[`updatePackages()`](https://andrie.github.io/miniCRAN/reference/updatePackages.md)

## Examples

``` r
### `checkVersions` and `add.packages.miniCRAN` require an existing miniCRAN repo

# Specify list of packages to download
mirror <- c(CRAN = "https://cloud.r-project.org")
mirror
#>                          CRAN 
#> "https://cloud.r-project.org" 
pkgs <- c("foreach")
pkgTypes <- c("source", "win.binary")

if (interactive()) {
  if (!is.online()) {
    message("p3m seems to be not available.  Check your internet connection.")
  } else {
    pdb <- pkgAvail(repos = mirror, type = "source")
  }
} else {
  pdb <- cranJuly2014
}


if (interactive()) {
  if (!is.online()) {
    message("p3m seems to be not available.  Check your internet connection.")
  } else {
    pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = mirror, type = "source", suggests = FALSE)
    pkgList
  }
}

# Create temporary folder for miniCRAN

if (interactive()) {
  if (!is.online()) {
    message("p3m seems to be not available.  Check your internet connection.")
  } else {
    dir.create(pth <- file.path(tempdir(), "miniCRAN"))
    
    # Make repo for source and win.binary
    makeRepo(pkgList, path = pth, repos = mirror, type = pkgTypes)
    
    # Add other versions of a package (and assume these were added previously)
    oldVers <- data.frame(
      package = c("foreach", "codetools", "iterators"),
      version = c("1.4.0", "0.2-7", "1.0.5"),
      stringsAsFactors = FALSE
    )
    pkgs <- oldVers$package
    addOldPackage(pkgs, path = pth, vers = oldVers$version, repos = mirror, type = "source")
    # NOTE: older binary versions would need to be build from source
    
    # List package versions in the miniCRAN repo (produces warning about duplicates)
    pkgVersionsSrc <- checkVersions(pkgs, path = pth, type = "source")
    pkgVersionsBin <- checkVersions(pkgs, path = pth, type = "win.binary")
    
    # After inspecting package versions, remove old versions
    basename(pkgVersionsSrc$source) # "foreach_1.4.0.tar.gz"  "foreach_1.4.2.tar.gz"
    basename(pkgVersionsBin$win.binary) # "foreach_1.4.0.zip"     "foreach_1.4.2.zip"
    file.remove(c(pkgVersionsSrc$source[1], pkgVersionsBin$win.binary[1]))
    
    # Rebuild package index after adding/removing files
    updateRepoIndex(pth, type = pkgTypes, Rversion = R.version)
    
    pkgAvail(pth, type = "source")
    
    # Add new packages (from CRAN) to the miniCRAN repo
    addPackage("Matrix", path = pth, repos = mirror, type = pkgTypes)
    
    # Delete temporary folder
    unlink(pth, recursive = TRUE)
  }
}
```
