
\dontrun{
pkgDep(pkg=c("ggplot2", "plyr", "reshape2"), 
       repos=c(CRAN="http://cran.revolutionanalytics.com")
)
}

pdb <- cranJuly2014
\dontrun{
pdb <- pkgAvail(repos=c(CRAN="http://cran.revolutionanalytics.com"))
}

pkgDep(pkg=c("ggplot2", "plyr", "reshape2"), pdb)

