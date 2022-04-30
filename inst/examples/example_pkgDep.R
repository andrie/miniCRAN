
if (interactive()) {
  pkgDep(pkg = c("ggplot2", "plyr", "reshape2"),
         repos = c(CRAN = "https://cloud.r-project.org")
  )
  
  pdb <- cranJuly2014
  pdb <- pkgAvail(repos = c(CRAN = getOption("minicran.mran")))
  
  pkgDep(pkg = c("ggplot2", "plyr", "reshape2"), pdb)
  
}
