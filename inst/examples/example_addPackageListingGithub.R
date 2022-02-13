# Create package database
pdb <- cranJuly2014

if (interactive()) {
  pdb <- pkgAvail(repos = c(CRAN = "https://cloud.r-project.org"))

  # Overwrite pdb with development version of miniCRAN at github
  newpdb <- addPackageListingGithub(pdb = pdb, "andrie/miniCRAN")
  newpdb["miniCRAN", ]

  # Add package from github that's not currently on CRAN
  newpdb <- addPackageListingGithub(pdb = pdb, repo = "tidyverse/ggplot2", branch = "main")
  newpdb["ggplot2", ]

  set.seed(1)
  plot(makeDepGraph("ggplot2", availPkgs = newpdb, suggests = TRUE))
}
