# Add DESCRIPTION information from package on github.

Downloads the DESCRIPTION file from a package on github, parses the
fields and adds (or replaces) a row in the available package database.

## Usage

``` r
addPackageListingGithub(
  pdb = pkgAvail(),
  repo,
  username = NULL,
  branch = "main"
)
```

## Arguments

- pdb:

  Package database, usually the result of
  [`pkgAvail()`](https://andrie.github.io/miniCRAN/reference/pkgAvail.md)
  or
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- repo:

  Character vector. Name of repository on github, e.g. `"andrie/rrd"`

- username:

  Optional character vector. Name of repository on github, e.g.
  `"andrie/rrd"`

- branch:

  name of branch, defaults to `"main"`

## Examples

``` r
# Create package database
pdb <- cranJuly2014

if (interactive()) {
  pdb <- pkgAvail(repos = c(CRAN = "https://cloud.r-project.org"))

  # Overwrite pdb with development version of miniCRAN at github
  newpdb <- addPackageListingGithub(pdb = pdb, "andrie/miniCRAN")
  newpdb["miniCRAN", ]

  # Add package from github that's not currently on CRAN
  newpdb <- addPackageListingGithub(pdb = pdb, repo = "tidyverse/ggplot2", branch = "main")
  newpdb["ggplot2", ]

  set.seed(1)
  plot(makeDepGraph("ggplot2", availPkgs = newpdb, suggests = TRUE))
}
```
