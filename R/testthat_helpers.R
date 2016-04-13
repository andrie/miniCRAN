# helper functions for testing

# Returns TRUE if a URL can be accessed
is.online <- function(url = MRAN()){
  z <- tryCatch(suppressWarnings(readLines(url, n = 10, warn = FALSE)),
                error = function(e)e
  )
  if(inherits(z, "error")) return(FALSE)
  TRUE
}

# Interrupt the test if url can not be reached
skip_if_offline <- function(url = MRAN()){
  if(!is.online(url)) testthat::skip("offline")
}

