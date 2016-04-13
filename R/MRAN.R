MRAN <- function(snapshot){
  url <- "http://mran.microsoft.com"
  if(missing("snapshot") || is.null(snapshot)){
  url
  } else {
    sprintf("%s/snapshot/%s", url, snapshot)
  }
}
CRAN <- function()getOption("repos")[1]
