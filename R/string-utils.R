#' Wrap elements
#'
#' @param x A vector.
#' @param wrap A string to wrap around vector elements
#' @return a vector with wrapped strings
#'
#' @export
wrap <- function (x, wrap = '"') {
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
trim <- function (x, trim = '\\s+') {
  stopifnot(is.vector(x))
  gsub(paste0("^", trim, "|", trim, "$"), '', x)
}


#' Duplicate a character string n times
#' 
#' @param x Input character string
#' @param n A numeric vector (number of times to duplicate) 
#' @return A character string
#' @export
dup <- function (x, n) {
  stopifnot(length(x) == 1L)
    vapply(Map(rep.int, rep.int(x, length(n)), n, USE.NAMES=FALSE),
         paste0, collapse="", character(1))
}


#' Dreate blank strings with a given number of characters
#' 
#' @usage blanks(n)
#' @param n A numeric vector (number of times to duplicate) 
#' @return A character vector
#' @seealso Examples for \code{\link{regmatches}}
#' @export
blanks <- Curry(FUN = dup, x = " ")


#' Pad a string
#' 
#' @param x Input character vector
#' @param n Pad \code{x} to this (minimum) width
#' @param where Side where the padding is added
#' @param pad Padding character
#' @return A character vector
#'
#' Slightly modified from Hadley Wickham's \code{\link[stringr]{str_pad}}
#' function
#'   
#' @export
pad <- function (x, n = 10, where = 'left', pad = ' ') {
  x <- as.character(x)
  stopifnot(length(n) == 1)
  stopifnot(length(where) == 1)
  stopifnot(length(pad) == 1)
  where <- match.arg(where, c("left", "right", "both"))
  needed <- pmax(0, n - nchar(x))
  left <- switch(where, left = needed, right = 0, both = floor(needed/2))
  right <- switch(where, left = 0, right = needed, both = ceiling(needed/2))
  lengths <- unique(c(left, right))
  padding <- dup(pad, lengths)
  paste0(padding[match(left, lengths)], x, padding[match(right, lengths)])
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

