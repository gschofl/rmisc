#' @include partial.R
NULL

#' Filter \code{NULL} entries from a list.
#' 
#' @param x A list.
#' @export
#' @examples
#' l <- list(a=1, b=NULL, c=NA)
#' compact(l)
#' ## $a
#' ## [1] 1
#' ## 
#' ## $c
#' ## [1] NA
compact <- function(x) {
  x[!vapply(x, is.null, FALSE, USE.NAMES=FALSE)]
}


#' Filter empty string entries from a list.
#' 
#' @param x A vector.
#' @export
compactChar <- function(x) {
  x[vapply(x, nzchar, FALSE, USE.NAMES=FALSE)]
}


#' Filter NA entries from a list.
#' 
#' @param x A vector.
#' @export
compactNA <- function(x) {
  filter <- Sequence(suppressWarnings%.%is.na, function(...) ... %||% FALSE)
  x[!vapply(x, filter, FALSE, USE.NAMES=FALSE)]
}

#' Filter empty entries from a list.
#' 
#' @param x A vector.
#' @export
compactAll <- function(x) {
  x[!vapply(x, are_empty, FALSE, USE.NAMES=FALSE)]
}
