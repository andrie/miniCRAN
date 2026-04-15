# List pre-built packages in a directory based on file extension

List pre-built packages in a directory based on file extension

## Usage

``` r
.listFiles(pkgs, path, type)
```

## Arguments

- pkgs:

  Character vector of package names

- path:

  Character string specifying the directory containing packages to be
  added.

- type:

  Character indicating the package type (e.g., "source", "win.binary",
  etc.).

## Value

Installs the packages and returns the new package index.

## Examples

``` r
if (FALSE) { # \dontrun{
 .listFiles('path/to/my/packages', type = "source")
} # }
```
