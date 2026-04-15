# Get a two-digit version of the R version

Get a two-digit version of the R version

## Usage

``` r
twodigitRversion(Rversion = R.version)
```

## Arguments

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

A character string representing the two-digit R version.
