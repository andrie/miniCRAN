# Downloads packages from CRAN to specified path and creates a local repository.

Given a list of packages, downloads these packages to a specified
destination folder using the required CRAN folder structure, and finally
creates the PACKAGES index file. Since the folder structure mimics the
required structure and files of a CRAN repository, it supports functions
like
[`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Usage

``` r
makeRepo(
  pkgs,
  path,
  repos = getOption("repos"),
  type = "source",
  Rversion = R.version,
  download = TRUE,
  writePACKAGES = TRUE,
  filters = NULL,
  quiet = FALSE
)

updateRepoIndex(path, type = "source", Rversion = R.version)
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

- download:

  If TRUE downloads packages.

- writePACKAGES:

  If TRUE, calls `write_PACKAGES()` to update the repository PACKAGES
  file.

- filters:

  passed to
  [utils::available.packages](https://rdrr.io/r/utils/available.packages.html)

- quiet:

  If TRUE, suppress status messages (if any), and the progress bar
  during download.

## Value

character vector of downloaded package files

## Note

Internally makes use of
[`utils::download.packages()`](https://rdrr.io/r/utils/download.packages.html)
and `write_PACKAGES()`

## Repo folder structure

A repository has two main folders, one for source packages, and the
other for binary packages. Inside the binary package folder, `bin`, you
will find subfolders for Windows as well as the various OSX binaries.

`+- Root`

`...+- src/contrib`

`......+- PACKAGES`

`..+- bin`

`.......+- windows/contrib/version`

`..........+- PACKAGES`

`.......+- macosx/contrib/version`

`..........+- PACKAGES`

`.......+- macosx/mavericks/contrib/version`

`..........+- PACKAGES`

`.......+- macosx/leopard/contrib/version`

`..........+- PACKAGES`

## See also

Other update repo functions:
[`addOldPackage()`](https://andrie.github.io/miniCRAN/reference/addOldPackage.md),
[`addPackage()`](https://andrie.github.io/miniCRAN/reference/addPackage.md),
[`checkVersions()`](https://andrie.github.io/miniCRAN/reference/checkVersions.md),
[`updatePackages()`](https://andrie.github.io/miniCRAN/reference/updatePackages.md)

## Examples

``` r
# Specify list of packages to download
mirror <- c(CRAN = "https://cloud.r-project.org")
pkgs <- c("foreach")

if (interactive()) {
  pdb <- cranJuly2014
  
  pdb <- pkgAvail(
    repos = c(CRAN = getOption("minicran.mran")),
    type = "source"
  )
  
  pkgList <- pkgDep(pkgs, availPkgs = pdb, repos = mirror,
                    type = "source", suggests = FALSE)
  pkgList
  
  
  # Create temporary folder for miniCRAN
  dir.create(pth <- file.path(tempdir(), "miniCRAN"))
  
  # Make repo for source and win.binary
  makeRepo(pkgList, path = pth, repos = mirror, type = "source")
  
  # List all files in miniCRAN
  list.files(pth, recursive = TRUE)
  
  # Check for available packages
  pkgAvail(repos = pth, type = "source")
  
  # Repeat process for windows binaries
  makeRepo(pkgList, path = pth, repos = mirror, type = "win.binary")
  pkgAvail(repos = pth, type = "win.binary")
  
  # Delete temporary folder
  unlink(pth, recursive = TRUE)
}
```
