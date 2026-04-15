# description

At the end of 2014, CRAN consisted of more than 6,000 packages. Many
organisations need to maintain a private mirror of CRAN, but with only a
subset of packages that are relevant to them.

## Details

`miniCRAN` makes it possible to create an internally consistent
repository consisting of selected packages from CRAN-like repositories.
The user specifies a set of desired packages, and miniCRAN recursively
reads the dependency tree for these packages, then downloads only this
subset.

There are many reasons for not creating a complete mirror CRAN using
`rsync`:

- You may wish to mirror only a subset of CRAN, for security, legal
  compliance or any other in-house reason

- You may wish to restrict internal package use to a subset of public
  packages, to minimize package duplication, or other reasons of coding
  standards

- You may wish to make packages available from public repositories other
  than CRAN, e.g. BioConductor, r-forge, OmegaHat, etc.

- You may wish to add custom in-house packages to your repository

The ambition of `miniCRAN` is to eventually satisfy all of these
considerations.

## Making a private repo

- [`pkgAvail()`](https://andrie.github.io/miniCRAN/reference/pkgAvail.md):
  Read from a local (or remote) CRAN-like repository and determine
  available packages.

- [`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md):
  Find (recursive) package dependencies.

- [`makeRepo()`](https://andrie.github.io/miniCRAN/reference/makeRepo.md)
  : Make a mini CRAN repository, by downloading packages (and their
  dependencies) and creating the appropriate file structure for a
  repository. This allows you to use functions like
  [`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html)
  and
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  on your local repository.

This subset will be internally consistent, i.e. the following functions
will work as expected:

- [`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html)

- [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)

The main function is
[`makeRepo()`](https://andrie.github.io/miniCRAN/reference/makeRepo.md) -
this will download all the required packages, with their dependencies,
into the appropriate repository file structure, and then create the
repository index (PACKAGES) file.

## Updating packages in a repo

- [`oldPackages()`](https://andrie.github.io/miniCRAN/reference/updatePackages.md):
  Indicates packages which have a (suitable) later version on the
  repositories \*
  [`updatePackages()`](https://andrie.github.io/miniCRAN/reference/updatePackages.md):
  Offers to download and install such packages

## Creating dependencies

To get a recursive list of dependencies as well as a plot, use
[`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md)
followed by
[`makeDepGraph()`](https://andrie.github.io/miniCRAN/reference/makeDepGraph.md).

- [`pkgDep()`](https://andrie.github.io/miniCRAN/reference/pkgDep.md):
  Find (recursive) package dependencies.

- [`makeDepGraph()`](https://andrie.github.io/miniCRAN/reference/makeDepGraph.md):
  Create graph of selected package dependencies.

- [`plot.pkgDepGraph()`](https://andrie.github.io/miniCRAN/reference/plot.pkgDepGraph.md):
  Create a visualization of the dependency graph

## Package options

- `minicran.mran`:

  preferred p3m URL. Defaults to <https://packagemanager.posit.co/cran>
  for R versions 3.2.2 and greater. Versions earlier than 3.2.2 use HTTP
  instead of HTTPS.

## See also

Useful links:

- <https://github.com/andrie/miniCRAN>

- Report bugs at <https://github.com/andrie/miniCRAN/issues>

## Author

**Maintainer**: Andrie de Vries <apdevries@gmail.com> \[copyright
holder\]

Other contributors:

- Alex Chubaty <alex.chubaty@gmail.com> \[contributor\]

- Microsoft Corporation \[copyright holder\]
