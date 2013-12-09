#' @include partial.R
#' @include compose.R
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
  vapply(.mapply(rep.int, list(rep.int(x, length(n)), n), NULL), paste0, collapse="", "")
}


#' Create blank strings with a given number of characters
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
#' @export
#' @examples
#' str <- c("Lorem, ipsum, dolor", "consectetur, adipisicing, elit")
#' strsplitN(str, ", ", 1)
#' ## [1] "Lorem"
#' ## [2] "consectetur"
#' strsplitN(str, ", ", 1, from="end")
#' ## [1] "dolor"
#' ## [2] "elit" 
#' strsplitN(str, ", ", 1:2)
#' ## [1] "Lorem, ipsum"
#' ## [2] "consectetur, adipisicing"
#' strsplitN(str, ", ", 1:2, collapse = "--")
#' ## [1] "Lorem--ipsum"
#' ## [2] "consectetur--adipisicing"
strsplitN <- function (x, split, n, from = "start", collapse = split, ...) {
  assert_that(is.vector(x))
  from <- match.arg(from, c("start", "end"))
  xs <- strsplit(x, split, ...)
  end <- vapply(xs, length, integer(1))
  
  if (from == "end") {
    end <- end + 1L
    n <- lapply(end, `-`, n)
    n <- .mapply(`[<-`, list(x=n, i=lapply(n, `<`, 0), value=0L), NULL)
  } else {
    n <- lapply(rep(0, length(xs)), `+`, n)
    n <- .mapply(`[<-`, list(x=n, i=Map(`>`, n, end), value=end), NULL)
  }  
  n <- lapply(n, Compose("sort", "unique"))
  unlist(.mapply(function(x, n) paste0(x[n], collapse = collapse), list(x = xs, n = n), NULL))
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


#' Strip file extensions
#'
#' Strips the extension (or an arbitrary tag) from a file name. 
#' 
#' @param file The file name(s).
#' @param sep specifies the seperator character (default ".").
#' @param level How many extensions should be stripped.
#' The default (0) strips all, 1 strips the last one, 2 strips the last two,
#' and so on.
#' 
#' @export
strip_ext <- stripExt <- function (file, sep="\\.", level=0) {
  assert_that(!missing(file), is.character(file))
  if (level == 0L) {
    # level 0 ditches everything that comes after a dot
    vapply(file, function(x) usp(x, sep)[1L], character(1),
           USE.NAMES = FALSE)
  } else if (level > 0L) {
    # level 1 removes the very last extension: file.xyz.abc > file.xyz
    # level 2: file.xyz.abc > file
    # and so on
    count <- count_re(file, sep) + 1L - level
    # to always grab at least the first element after the split
    # reset zero counts to 1
    count <- ifelse(count < 1, 1, count)
    unlist(Map(function(x, lvl) {
      paste0(usp(x, sep)[ seq_len(lvl) ], collapse = gsub('\\', '', sep, fixed=TRUE))
    }, x=file, lvl=count, USE.NAMES=FALSE))
  } else {
    stop(sprintf("Level %s is invalid. Must be 0, 1, 2, ...", sQuote(level)))
  }
}


#' Replace file extensions
#' 
#' @inheritParams strip_ext
#' @param replacement replacement extension
#'
#' @export
replace_ext <- replaceExt <- function (file, replacement="", sep="\\.", level=0) {
  if (nchar(replacement) == 0L)
    sep=""
  # strip a leading "." from replacement
  if (grepl("^\\.", replacement)) {
    replacement <- usp(replacement, split="^\\.")[2L]
  }
  paste(strip_ext(file=file, sep=sep, level=level), replacement,
        sep=gsub("\\", "", sep, fixed=TRUE))  
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


#' unlist(strsplit(x, split, ...))
#' 
#' @usage usplit(x, split, ...)
#' @param x Character vector to be split.
#' @param split The regexp to split on.
#' @param \dots Arguments passed on to \code{\link{strsplit}}.
#' @export
#' @examples
#' usp("a.b.c", ".", fixed = TRUE)
## ## [1] "a" "b" "c"
usplit <- Compose("unlist", "strsplit")


