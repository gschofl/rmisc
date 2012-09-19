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


#' Split up a string in pieces and return the nth piece.
#' 
#' @param x character vector to be split.
#' @param split regular expression used for splitting.
#' @param n piece to be returned after the split. Can be a vector.
#' @param from One of \sQuote{start} or \sQuote{end}. From which direction do
#' we count the pieces.
#' @param ... Arguments passed on to \code{\link{strsplit}}.
#' @return A character vector
#' 
#' @export
strsplitN <- function (x, split, n = 3, from = c("start", "end"), ...) {
  stopifnot(is.vector(x))
  from <- match.arg(from)
  if (from == "end") {
    n <- vapply(strsplit(x, split, ...), length, numeric(1)) - n + 1
    n[n < 0] <- 0
  } else {
    n <- c(rep(n, length(x) %/% length(n)), n[seq_len(length(x) %% length(n))])
  }
  unlist(Map(function(x, n) x[n], x=strsplit(x, split, ...), n=n))
}

