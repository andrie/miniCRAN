# Reads available packages from CRAN repository.

This is a thin wrapper around
[`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html).
If the argument `path` is supplied, then the function attempts to read
from a local repository, otherwise attempts to read from a CRAN mirror
at the `repos` url.

## Usage

``` r
pkgAvail(
  repos = getOption("repos"),
  type = "source",
  Rversion = R.version,
  quiet = FALSE,
  filters = NULL
)
```

## Arguments

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

- quiet:

  If TRUE, suppresses warnings

- filters:

  passed to
  [utils::available.packages](https://rdrr.io/r/utils/available.packages.html)

## See also

[`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md)
