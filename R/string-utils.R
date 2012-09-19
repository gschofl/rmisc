#' Wrap elements
#'
#' @param x A vector.
#' @param wrap A string to wrap around vector elements
#' @return a vector with wrapped strings
#'
#' @export
wrap <- function(x, wrap = '"') {
    stopifnot(is.vector(x))
    sprintf('%s%s%s', wrap, x, wrap)
}

#' Trim elements
#' 
#' @param x A vector.
#' @param trim A pattern to trim from vector elemts
#' @return A vector with trimmed strings
#'
#'@export
trim <- function(x, trim = '\\s+') {
  stopifnot(is.vector(x))
  gsub(paste0("^", trim, "|", trim, "$"), '', x)
}

