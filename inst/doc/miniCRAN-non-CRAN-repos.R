## ----setup---------------------------------------------------------------
# Wrapper around available.packages ---------------------------------------
 
index <- function(url, type="source", filters=NULL, head=5, cols=c("Package", "Version")){
  contribUrl <- contrib.url(url, type=type)
  p <- available.packages(contribUrl, type=type, filters=filters)
  p[1:head, cols]
}
 

## ----CRAN----------------------------------------------------------------
CRAN <- "http://cran.r-project.org"
index(CRAN)

## ----revo----------------------------------------------------------------
revoStable <- "http://packages.revolutionanalytics.com/cran/3.1/stable"
index(revoStable)
 
revoMirror <- "http://cran.revolutionanalytics.com"
index(revoMirror)

## ----rforge--------------------------------------------------------------
rforge <- "http://r-forge.r-project.org"
index(rforge)

## ----bioc----------------------------------------------------------------
bioc <- local({
  on.exit(rm(env))
  env <- new.env()
  evalq(source("http://bioconductor.org/biocLite.R", local=TRUE), env)
  biocinstallRepos()
})
 
bioc
bioc[grep("BioC", names(bioc))]
 
 
index(bioc["BioCsoft"])

