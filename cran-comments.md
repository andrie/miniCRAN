This is a release with no functional changes but fixes failing tests on CRAN, and to comply with CRAN policy.

In particular, I have reviewed all examples to run in less than 5 seconds, and tests to run only if internet connection is available, and to print informative messages if not.

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.


## Downstream dependencies

`miniCRAN` has reverse dependencies  (`deepdep`), and all tests pass.