This release removes a dependency on XML that caused build problems on old versions of R.

## Test environments

* local Windows install, R04.0.2
* ubuntu xenial 16.04 (on github actions), testing on:
  - R-release
  - R-oldrel
  - R-devel
  - R-3.5
  - R-3.4
  - R-3.3
* Mac OS (on github actions)
  - R-devel
  - R-release
* Windows (on github actions)
  - R-devel
  - R-release
  

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.


## Downstream dependencies

`miniCRAN` has only one reverse dependency, `deepdep`, and all tests pass