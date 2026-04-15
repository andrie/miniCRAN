# Check for available package updates in a miniCRAN repo.

`oldPackages()` indicates packages which have a (suitable) later version
on the repositories whereas `updatePackages()` offers to download and
install such packages.

## Usage

``` r
oldPackages(
  path = NULL,
  repos = getOption("repos"),
  availPkgs = pkgAvail(repos = repos, type = type, Rversion = Rversion),
  method,
  availableLocal = pkgAvail(repos = path, type = type, Rversion = Rversion, quiet =
    quiet),
  type = "source",
  Rversion = R.version,
  quiet = FALSE
)

updatePackages(
  path = NULL,
  repos = getOption("repos"),
  method = NULL,
  ask = TRUE,
  availPkgs = pkgAvail(repos = repos, type = type, Rversion = Rversion),
  oldPkgs = NULL,
  type = "source",
  Rversion = R.version,
  quiet = FALSE
)
```

## Arguments

- path:

  Destination download path. This path is the root folder of your new
  repository.

- repos:

  URL(s) of the 'contrib' sections of the repositories, e.g.
  `"https://cran.us.r-project.org"`. Passed to
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- availPkgs:

  Data frame with an element called `package`. The `package` element is
  a vector of available packages. Defaults to reading this list from
  CRAN, using
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- method:

  Download method, see
  [`download.file()`](https://rdrr.io/r/utils/download.file.html).

- availableLocal:

  all packages hosted in the miniCRAN repo, as returned by
  [`pkgAvail()`](https://andrie.github.io/miniCRAN/reference/pkgAvail.md).
  A subset can be specified; currently this must be in the same
  (character matrix) format as returned by
  [`pkgAvail()`](https://andrie.github.io/miniCRAN/reference/pkgAvail.md).

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

- quiet:

  If TRUE, suppress status messages (if any), and the progress bar
  during download.

- ask:

  logical indicating whether to ask user before packages are actually
  downloaded and installed. Alternatively, the value `"graphics"` starts
  an interactive widget to allow the user to (de-)select from the list
  of packages which could be updated or added. The latter value only
  works on systems with a GUI version of
  [`select.list()`](https://rdrr.io/r/utils/select.list.html), and is
  otherwise equivalent to `ask = TRUE`.

- oldPkgs:

  if specified as non-NULL, `updatePackages()` only considers these
  packages for updating. This may be a character vector of package names
  or a matrix as returned by `oldPackages()`.

## Value

`oldPackages()` returns a matrix with one row per package and columns
for "Package", "LocalVer", "ReposVer" and "Repository". The matrix row
names the package names.

`updatePackages` returns `NULL` invisibly.

## Details

These functions are based on
[`update.packages()`](https://rdrr.io/r/utils/update.packages.html).
However, rather than looking for locally installed packages they look
for the package source and binaries in the miniCRAN repository.

## See also

`updatePackages()`,
[`pkgAvail()`](https://andrie.github.io/miniCRAN/reference/pkgAvail.md).

Other update repo functions:
[`addOldPackage()`](https://andrie.github.io/miniCRAN/reference/addOldPackage.md),
[`addPackage()`](https://andrie.github.io/miniCRAN/reference/addPackage.md),
[`checkVersions()`](https://andrie.github.io/miniCRAN/reference/checkVersions.md),
[`makeRepo()`](https://andrie.github.io/miniCRAN/reference/makeRepo.md)

## Examples

``` r
### `oldPackages` and `updatePackages` require an existing miniCRAN repo

# Specify list of packages to download
mirror <- c(CRAN = "https://cloud.r-project.org")
pkgs <- c("foreach")

pdb <- cranJuly2014

if (interactive()) {
  pdb <- pkgAvail(repos = mirror, type = "source")
  
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = mirror, type = "source", suggests = FALSE)
  pkgList
  
  # Create temporary folder for miniCRAN
  dir.create(pth <- file.path(tempdir(), "miniCRAN"))
  
  # create the miniCRAN directory structure but only add bin files
  makeRepo(pkgList, path = pth, repos = mirror, type = "source", download = FALSE)
  makeRepo(pkgList, path = pth, repos = mirror, type = "win.binary", download = TRUE)
  
  # download old source package version and create repo index
  oldVers <- data.frame(package = c("foreach", "codetools", "iterators"),
                        version = c("1.4.0", "0.2-7", "1.0.5"),
                        stringsAsFactors = FALSE)
  addOldPackage(pkgList, path = pth, repos = mirror, vers = oldVers$version, type = "source")
  # NOTE: older binary versions would need to be build from source
  
  # Check if updated packages are available
  oldPackages(path = pth, repos = mirror, type = "source") # should need update
  oldPackages(path = pth, repos = mirror, type = "win.binary") # should be current
  
  # Update available packages
  updatePackages(path = pth, repos = mirror, type = "source", ask = FALSE) # should need update
  updatePackages(path = pth, repos = mirror, type = "win.binary") # should be current
  
  # Delete temporary folder
  unlink(pth, recursive = TRUE)
}
```
