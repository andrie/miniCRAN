# Deprecated function to download packages to local folder.

Deprecated function to download packages to local folder.

## Usage

``` r
makeLibrary(pkgs, path, type = "source")
```

## Arguments

- pkgs:

  Character vector of packages to download

- path:

  Destination download path. This path is the root folder of your new
  repository.

- type:

  Possible values are (currently) "source", "mac.binary" and
  "win.binary": the binary types can be listed and downloaded but not
  installed on other platforms. Passed to
  [`download.packages()`](https://rdrr.io/r/utils/download.packages.html).
