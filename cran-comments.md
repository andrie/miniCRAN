## Test environments

* local Windows install, R-3.4.3
* ubuntu trusty 14.04 (on travis-ci), testing on:
  - R-release
  - R-oldrel
  - R-devel
* XCode (Mac OS) (on travis-ci)

## R CMD check results

There were no ERRORs or WARNINGs.

There is a NOTE on old releases, that `tools::CRAN_package_db()` is not exported.
In my code I use `if (getRversion() >= "3.4.1") {...}` to test for this condition, and only refer to `CRAN_package_db()` in more recent releases.

## Downstream dependencies

`miniCRAN` has only one reverse dependency, `AzureML`, and all tests pass