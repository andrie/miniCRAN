pkgDep(pkg=c("ggplot2", "plyr", "reshape2"), 
       repos=c(CRAN="http://cran.revolutionanalytics.com")
)


pdb <- pkgAvail()
pkgDep(pkg=c("ggplot2", "plyr", "reshape2"), pdb)

