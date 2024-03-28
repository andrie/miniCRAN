## ----setup--------------------------------------------------------------------
# Wrapper around available.packages ---------------------------------------
 
index <- function(url, type = "source", filters = NULL, head = 5, 
                  cols = c("Package", "Version")) {
  contribUrl <- contrib.url(url, type = type)
  p <- available.packages(contribUrl, type = type, filters = filters)
  p[1:head, cols]
}
 

## ----CRAN, eval=FALSE---------------------------------------------------------
#  CRAN <- "https://cran.r-project.org"
#  index(CRAN)

## ----p3m, eval=FALSE----------------------------------------------------------
#  p3m <- "https://packagemanager.posit.co/cran/2024-01-02"
#  index(p3m)

## ----rforge, eval=FALSE-------------------------------------------------------
#  rforge <- "https://r-forge.r-project.org"
#  index(rforge)

