
# Create package database

pdb <- cranJuly2014

\dontrun{
  pdb <- pkgAvail(repos=c(CRAN="http://cran.revolutionanalytics.com"))
}

# Overwrite pdb with development version of miniCRAN at github
\dontrun{
  newpdb <- addPackageGithub(pdb=pdb, "andrie/miniCRAN")
  newpdb["miniCRAN", ]
}

# Add package from github that's not currently on CRAN
newpdb <- addPackageGithub(pdb=pdb, repo="RevolutionAnalytics/RRT")
newpdb["RRT", ]

set.seed(1)
plot(makeDepGraph("RRT", availPkgs = newpdb, suggests=TRUE), 
     legendPosition=c(1, -0.7))





