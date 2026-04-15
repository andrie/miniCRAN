# Package index

## Making a private repo

- [`pkgAvail()`](https://andrie.github.io/miniCRAN/reference/pkgAvail.md)
  : Reads available packages from CRAN repository.
- [`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md) :
  Retrieves package dependencies.
- [`makeRepo()`](https://andrie.github.io/miniCRAN/reference/makeRepo.md)
  [`updateRepoIndex()`](https://andrie.github.io/miniCRAN/reference/makeRepo.md)
  : Downloads packages from CRAN to specified path and creates a local
  repository.

## Updating packages in a repo

- [`oldPackages()`](https://andrie.github.io/miniCRAN/reference/updatePackages.md)
  [`updatePackages()`](https://andrie.github.io/miniCRAN/reference/updatePackages.md)
  : Check for available package updates in a miniCRAN repo.
- [`addLocalPackage()`](https://andrie.github.io/miniCRAN/reference/addLocalPackage.md)
  : Add local packages to a miniCRAN repository.
- [`addOldPackage()`](https://andrie.github.io/miniCRAN/reference/addOldPackage.md)
  : Add old package versions to a miniCRAN repository.
- [`addPackage()`](https://andrie.github.io/miniCRAN/reference/addPackage.md)
  : Add packages to a miniCRAN repository.

## Dependency functions

- [`basePkgs()`](https://andrie.github.io/miniCRAN/reference/basePkgs.md)
  : Returns names of base packages.

## Github functions

- [`addPackageListingGithub()`](https://andrie.github.io/miniCRAN/reference/addPackageListingGithub.md)
  : Add DESCRIPTION information from package on github.

## Creating dependencies

- [`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md) :
  Retrieves package dependencies.
- [`makeDepGraph()`](https://andrie.github.io/miniCRAN/reference/makeDepGraph.md)
  : Create dependency graph from available packages.
- [`plot(`*`<pkgDepGraph>`*`)`](https://andrie.github.io/miniCRAN/reference/plot.pkgDepGraph.md)
  : Plots a package dependency graph.

## Deprecated

- [`makeLibrary()`](https://andrie.github.io/miniCRAN/reference/makeLibrary.md)
  : Deprecated function to download packages to local folder.

## Other

- [`checkVersions()`](https://andrie.github.io/miniCRAN/reference/checkVersions.md)
  : Check for previous versions of packages in a miniCRAN repository.
- [`cranJuly2014`](https://andrie.github.io/miniCRAN/reference/cranJuly2014.md)
  : Stored version of available.packages()
- [`getCranDescription()`](https://andrie.github.io/miniCRAN/reference/getCranDescription.md)
  : Obtains DESCRIPTION metadata from CRAN for each package.
- [`.listFiles()`](https://andrie.github.io/miniCRAN/reference/listFiles.md)
  : List pre-built packages in a directory based on file extension
- [`miniCRAN-package`](https://andrie.github.io/miniCRAN/reference/miniCRAN-package.md)
  [`miniCRAN`](https://andrie.github.io/miniCRAN/reference/miniCRAN-package.md)
  [`minicran`](https://andrie.github.io/miniCRAN/reference/miniCRAN-package.md)
  : description
- [`repoPrefix()`](https://andrie.github.io/miniCRAN/reference/repoPrefix.md)
  : Get the path to the repo directory containing the package files.
- [`twodigitRversion()`](https://andrie.github.io/miniCRAN/reference/twodigitRversion.md)
  : Get a two-digit version of the R version
- [`is.online()`](https://andrie.github.io/miniCRAN/reference/is.online.md)
  : Returns TRUE if the p3m URL can be accessed.
