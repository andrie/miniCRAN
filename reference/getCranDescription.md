# Obtains DESCRIPTION metadata from CRAN for each package.

This is a wrapper around
[`tools::CRAN_package_db`](https://rdrr.io/r/tools/CRANtools.html) and
may be deprecated in future versions of the package.

## Usage

``` r
getCranDescription(
  pkg,
  repos = getOption("repos"),
  type = "source",
  pkgs = pkgDep(pkg, repos = repos, type = type)
)
```

## Arguments

- pkg:

  Character vector of packages.

- repos:

  URL(s) of the 'contrib' sections of the repositories, e.g.
  `"https://cran.us.r-project.org"`. Passed to
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- type:

  Possible values are (currently) "source", "mac.binary" and
  "win.binary": the binary types can be listed and downloaded but not
  installed on other platforms. Passed to
  [`download.packages()`](https://rdrr.io/r/utils/download.packages.html).

- pkgs:

  Character vector of packages to download

## Examples

``` r
if (interactive()) {
  getCranDescription(c("igraph", "ggplot2", "XML"),
                     repos = c(CRAN = getOption("minicran.mran"))
  )
}
```
