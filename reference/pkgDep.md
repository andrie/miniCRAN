# Retrieves package dependencies.

Performs recursive retrieve for `Depends`, `Imports` and `LinkLibrary`.
Performs non-recursive retrieve for `Suggests`.

## Usage

``` r
pkgDep(
  pkg,
  availPkgs,
  repos = getOption("repos"),
  type = "source",
  depends = TRUE,
  suggests = TRUE,
  enhances = FALSE,
  includeBasePkgs = FALSE,
  Rversion = R.version,
  quiet = FALSE,
  ...
)
```

## Arguments

- pkg:

  Character vector of packages.

- availPkgs:

  Data frame with an element called `package`. The `package` element is
  a vector of available packages. Defaults to reading this list from
  CRAN, using
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- repos:

  URL(s) of the 'contrib' sections of the repositories, e.g.
  `"https://cran.us.r-project.org"`. Passed to
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- type:

  Possible values are (currently) "source", "mac.binary" and
  "win.binary": the binary types can be listed and downloaded but not
  installed on other platforms. Passed to
  [`download.packages()`](https://rdrr.io/r/utils/download.packages.html).

- depends:

  If TRUE, retrieves `Depends`, `Imports` and `LinkingTo` dependencies
  (non-recursively)

- suggests:

  If TRUE, retrieves Suggests dependencies (non-recursively)

- enhances:

  If TRUE, retrieves Enhances dependencies (non-recursively)

- includeBasePkgs:

  If TRUE, include base R packages in results

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

- ...:

  Other arguments passed to
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

## Value

character vector of package names

## See also

Other dependency functions:
[`basePkgs()`](https://andrie.github.io/miniCRAN/reference/basePkgs.md),
[`makeDepGraph()`](https://andrie.github.io/miniCRAN/reference/makeDepGraph.md),
[`plot.pkgDepGraph()`](https://andrie.github.io/miniCRAN/reference/plot.pkgDepGraph.md)

## Examples

``` r
if (interactive()) {
  pkgDep(pkg = c("ggplot2", "plyr", "reshape2"),
         repos = c(CRAN = "https://cloud.r-project.org")
  )
  
  pdb <- cranJuly2014
  pdb <- pkgAvail(repos = c(CRAN = getOption("minicran.mran")))
  
  pkgDep(pkg = c("ggplot2", "plyr", "reshape2"), pdb)
  
}
```
