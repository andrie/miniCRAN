#' @importFrom assertthat assert_that on_failure<-

# path --------------------------------------------------------------------

is_path <- function(x) {
  is.character(x) && length(x) == 1
}

on_failure(is_path) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid path")
}


# pkg ---------------------------------------------------------------------

is_package <- function(x) {
  is.character(x) && length(x) > 0
}

on_failure(is_package) <- function(call, env) {
  paste0(deparse(call$x), "pkg should be a character vector with package names")
}


