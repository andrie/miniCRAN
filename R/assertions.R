#' @importFrom assertthat assert_that on_failure<-

# path --------------------------------------------------------------------

is_path <- function(x) {
  is.character(x) && length(x) == 1
}

on_failure(is_path) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid path")
}

path_exists <- function(x) {
  file.exists(x)
}

on_failure(path_exists) <- function(call, env) {
  paste0("Download path ", deparse(call$x), " does not exist")
}


# pkg ---------------------------------------------------------------------

is_package <- function(x) {
  is.character(x) && length(x) > 0
}

on_failure(is_package) <- function(call, env) {
  paste0(deparse(call$x), " should be a character vector with package names")
}

is_package_vector <- is_package


# repos -------------------------------------------------------------------

is_repos <- function(x) {
  is.character(x) && length(x) > 0
}

on_failure(is_repos) <- function(call, env) {
  paste0(deparse(call$x), " should be a character vector")
}
