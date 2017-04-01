# helper functions for testing

# Returns TRUE if a URL can be accessed
is.online <- function(url = MRAN(), tryHttp = TRUE){
  readFromUrl <- function(url){
    z <- tryCatch(suppressWarnings(readLines(url, n = 1, warn = FALSE)),
                  error = function(e)e
    )
    !inherits(z, "error")
  }
  
  if(!readFromUrl(url)){
    if(grepl("^https://", url) && tryHttp){
      url <- sub("^https://", "http://", url) # for older versions of R
      if(!readFromUrl(url))
        return(FALSE)
    } else {
      return(FALSE)
    }
  }
  
  TRUE
}

# Interrupt the test if url can not be reached
skip_if_offline <- function(url = MRAN()){
  if(!is.online(url)) testthat::skip("offline")
}

