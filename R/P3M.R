p3m <- function(snapshot = NULL) {
  url <- getOption("minicran.mran")
  if (missing("snapshot") || is.null(snapshot)) {
    url
  } else {
    sprintf("%s/%s", url, snapshot)
  }
}

CRAN <- function() getOption("repos")[1]


#' Returns TRUE if the p3m URL can be accessed.
#'
#' @param url p3m url
#' @param tryHttp If TRUE, also attempts http URL, for compatibility with older versions of R
#'
#' @export
is.online <- function(url = NULL, tryHttp = TRUE) {
  if (is.null(url)) url <- p3m()
  url <- sub("latest$", "", url)
  url <- sub("\\d{4}-\\d{2}-\\d{2}$", "", url)

  readFromUrl <- function(url) {
    z <- tryCatch(
      suppressWarnings(readLines(url, n = 1, warn = FALSE)),
      error = function(e) e
    )
    !inherits(z, "error")
  }

  if (!readFromUrl(url)) {
    if (grepl("^https://", url) && tryHttp) {
      url <- sub("^https://", "http://", url) # for older versions of R
      if (!readFromUrl(url)) return(FALSE)
    } else {
      return(FALSE)
    }
  }

  TRUE
}
