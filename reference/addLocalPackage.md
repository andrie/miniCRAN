# Add local packages to a miniCRAN repository.

Examine the contents of a directory specified by `pkgPath` for pre-built
packages matching the names specified by `pkgs`, and add these to the
miniCRAN repository.

## Usage

``` r
addLocalPackage(
  pkgs = NULL,
  pkgPath = NULL,
  path = NULL,
  type = "source",
  Rversion = R.version,
  writePACKAGES = TRUE,
  deps = FALSE,
  quiet = FALSE,
  build = FALSE
)
```

## Arguments

- pkgs:

  Character vector of packages to download

- pkgPath:

  Character vector of directory location containing packages to be
  added. Note that `pkgPath` should be the parent directory of the
  package (i.e., the package directory path is constructed from
  `file.path(pkgPath, pkgs)`).

- path:

  Destination download path. This path is the root folder of your new
  repository.

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

  Not used. See note.

- quiet:

  If TRUE, suppress status messages (if any), and the progress bar
  during download.

- build:

  Logical indicating whether packages should be build prior to adding.

## Value

Installs the packages and returns the new package index.

## Details

To build a package from source and then add it, use `build = TRUE`. Note
that package development libraries and the `devtools` package must be
installed on your system in order to build packages.

## Note

Currently, adding local packages does not check nor download their
dependencies.

## Author

Alex Chubaty

## Examples

``` r
if (FALSE) { # \dontrun{
 addLocalPackage("myPackage", "path/to/my/prebuilt/package",
                 "path/to/my/miniCRAN/repo")

 addLocalPackage("myPackage", "path/to/my/package/sourcecode",
                 "path/to/my/miniCRAN/repo", build = TRUE)
} # }
```
