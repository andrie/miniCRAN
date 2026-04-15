# Returns names of base packages.

Retrieves names of installed packages by calling
[`utils::installed.packages()`](https://rdrr.io/r/utils/installed.packages.html)
and returning only those packages where `Priority == "base"`.

## Usage

``` r
basePkgs()
```

## See also

[`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md)

Other dependency functions:
[`makeDepGraph()`](https://andrie.github.io/miniCRAN/reference/makeDepGraph.md),
[`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md),
[`plot.pkgDepGraph()`](https://andrie.github.io/miniCRAN/reference/plot.pkgDepGraph.md)
