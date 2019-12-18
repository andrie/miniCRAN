#' @param Rversion Version of R (only used if `type` is not `source`.) Defaults to [R.version], but this can be specified as any of the following formats:
#'
#'   * a character string with the two digit R version, e.g. "3.1"
#'   * a list with components `major` and `minor`
#'   * the result of [getRversion()]
#'   * the result of [R.version]
