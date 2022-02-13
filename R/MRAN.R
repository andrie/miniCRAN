MRAN <- function(snapshot = NULL) {
  url <- getOption("minicran.mran")
  if (missing("snapshot") || is.null(snapshot)) {
    url
  } else {
    sprintf("%s/snapshot/%s", url, snapshot)
  }
}
CRAN <- function() getOption("repos")[1]


#' Returns TRUE if the MRAN URL can be accessed.
#' 
#' @param url MRAN url
#' @param tryHttp If TRUE, also attempts http URL, for compatibility with older versions of R
#' 
#' @export
is.online <- function(url = NULL, tryHttp = TRUE) {
  if (is.null(url)) url <- MRAN()
  
  readFromUrl <- function(url){
    z <- tryCatch(suppressWarnings(readLines(url, n = 1, warn = FALSE)),
                  error = function(e) e
    )
    !inherits(z, "error")
  }
  
  if (!readFromUrl(url)) {
    if (grepl("^https://", url) && tryHttp) {
      url <- sub("^https://", "http://", url) # for older versions of R
      if (!readFromUrl(url))
        return(FALSE)
    } else {
      return(FALSE)
    }
  }
  
  TRUE
}
