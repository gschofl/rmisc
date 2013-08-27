#' @include functional.R
NULL

#' Wrap elements
#'
#' @param x A vector.
#' @param wrap A string to wrap around vector elements
#' @return a vector with wrapped strings
#'
#' @export
#' @examples
#'   wrap(c("abc", "xyz"), "---")
wrap <- function (x, wrap = '"') {
  assert_that(is.vector(x))
  sprintf('%s%s%s', wrap, x, wrap)
}

#' Trim elements
#' 
#' @param x A vector.
#' @param trim A pattern to trim from vector elements
#' @return A vector with trimmed strings
#'
#' @export
#' @examples
#'  x <- " abc\n\t" 
#'  trim(x)
trim <- function (x, trim = '\\s+') {
  assert_that(is.vector(x))
  gsub(paste0("^", trim, "|", trim, "$"), '', x)
}


#' Duplicate a character string n times
#' 
#' @param x Input character string
#' @param n A numeric vector (number of times to duplicate) 
#' @return A character string
#' @export
#' @examples
#'   dup("#", 1:10)
dup <- function (x, n) {
  assert_that(is.string(x))
  if (any(n < 0))
    n[n < 0] <- 0
  vapply(Map(rep.int, rep.int(x, length(n)), n, USE.NAMES=FALSE),
         paste0, collapse="", character(1))
}


#' Dreate blank strings with a given number of characters
#' 
#' @usage blanks(n)
#' @param n A numeric vector (number of times to duplicate) 
#' @return A character vector
#' @export
#' @examples
#'  blanks(10)
blanks <- Partial(dup, x = " ")


#' Pad a string
#' 
#' @param x Input character vector.
#' @param n Pad \code{x} to this (minimum) width.
#' @param where Side where the padding is added.
#' @param pad Padding character.
#' @return A character vector.
#' @export
pad <- function (x, n = 10, where = 'left', pad = ' ') {
  x <- as.character(x)
  assert_that(is.scalar(n), is.scalar(where), is.scalar(pad))
  where <- match.arg(where, c("left", "right", "both"))
  needed <- pmax(0, n - nchar(x))
  left <- switch(where, left = needed, right = 0, both = floor(needed/2))
  right <- switch(where, left = 0, right = needed, both = ceiling(needed/2))
  lengths <- unique(c(left, right))
  padding <- dup(pad, lengths)
  paste0(padding[match(left, lengths)], x, padding[match(right, lengths)])
}


#' unlist(strsplit(x, split, ...))
#' 
#' @usage usp(x, split, ...)
#' @param x Character vector to be split.
#' @param split The regexp to split on.
#' @param \dots Arguments passed on to \code{\link{strsplit}}.
#' @export
#' @examples
#' usp("a.b.c", ".", fixed = TRUE)
## ## [1] "a" "b" "c"
usp <- Compose("unlist", "strsplit")


#' Split up a string in pieces and return the nth piece.
#' 
#' @param x character vector to be split.
#' @param split regular expression used for splitting.
#' @param n Vector of elements to be returned.
#' @param from One of \sQuote{start} or \sQuote{end}. From which direction do
#' we count the pieces.
#' @param collapse
#' @param ... Arguments passed on to \code{\link{strsplit}}.
#' @return A character vector.
#' 
#' @export
strsplitN <- function (x, split, n, from = "start", collapse = split, ...) {
  assert_that(is.vector(x))
  from <- match.arg(from, c("start", "end"))
  xs <- strsplit(x, split, ...)
  end <- vapply(xs, length, integer(1))
  
  if (from == "end") {
    end <- end + 1L
    n <- lapply(end, `-`, n)
    n <- Map(`[<-`, x=n, i=lapply(n, `<`, 0), value=0L)
  } else {
    n <- lapply(rep(0, length(xs)), `+`, n)
    n <- Map(`[<-`, x=n, i=Map(`>`, n, end), value=end)
  }  
  n <- lapply(n, Compose("sort", "unique"))
  mapply(function(x, n) paste0(x[n], collapse = collapse), x = xs, n = n)
}


#' Split a file path and return the nth piece(s)
#'
#' @param path Vector of file paths.
#' @param n Which part(s) to return.
#' @param from One of \sQuote{start} or \sQuote{end}. From which direction do
#' we count the parts.
#' @param ... Arguments passed on to \code{\link{strsplitN}}.
#'
#' @export
split_path <- function (path, n = 1, from = "end", ...) {
  from <- match.arg(from, c("start", "end"))
  strsplitN(x=path, split=.Platform$file.sep, n=n, from=from, ...)
}


#' Check if a pattern occurs in a character vector
#' 
#' @param x Character vector.
#' @param re Regular expression pattern passed to \code{grep}.
#' @param ... Additional arguments passed to \code{\link{grep}}.
#' @export
#' @examples
#' exists_re(c("foo", "bar", "baz"), "^b")
exists_re <- function(x, re, ...) {
  if (length(x) == 1)
    grepl(re, x, ...)
  else
    vapply(x, grepl, pattern = re, ..., FUN.VALUE=logical(1), USE.NAMES=FALSE)
}


#' Count how often a pattern occurs in a character vector.
#' 
#' @param x Character vector.
#' @param re Regular expression pattern passed to \code{gregexpr}.
#' @param ... Additional arguments passed to \code{\link{gregexpr}}.
#' @export
#' @examples
#' count_re(c("foo", "bar", "baz"), "^b")
count_re <- function(x, re, ...) {
  vapply(gregexpr(re, x, ...), function (x) sum(x > 0L), numeric(1L),
  USE.NAMES=FALSE)
}

