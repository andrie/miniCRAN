# Get the path to the repo directory containing the package files.

Get the path to the repo directory containing the package files.

## Usage

``` r
repoPrefix(type, Rversion)
```

## Arguments

- type:

  character, indicating the type of package to download and install. See
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html).

- Rversion:

  Version of R (only used if `type` is not `source`.) Defaults to
  [R.version](https://rdrr.io/r/base/Version.html), but this can be
  specified as any of the following formats:

  - a character string with the two digit R version, e.g. "3.1"

  - a list with components `major` and `minor`

  - the result of
    [`getRversion()`](https://rdrr.io/r/base/numeric_version.html)

  - the result of [R.version](https://rdrr.io/r/base/Version.html)

## Value

The file path to the package files directory.

## Note

Not all versions of R are compatible with with all package types (e.g.,
`mac.binary.el-capitan` is only valid for R \> 3.4.0).

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
