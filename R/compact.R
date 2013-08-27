#' @include partial.R
NULL

#' Filter entries from a list.
#' 
#' @param x A list.
#' @param filter A function to filter entries. (default \code{is.null})
#' @export
#' @examples
#' l <- list(a=1, b=NULL, c=NA)
#' compact(l)
#' ## $a
#' ## [1] 1
#' ## 
#' ## $c
#' ## [1] NA
compact <- function (x, filter = "is.null") {
  filter <- match.fun(filter)
  x[!vapply(x, filter, logical(1), USE.NAMES=FALSE)]
}


#' Filter NA entries from a list.
#' 
#' @param x A vector.
#' @export
compactNA <- Partial(compact, filter = "is.na")


#' Filter empty entries from a list.
#' 
#' @param x A vector.
#' @export
compactAll <- Partial(compact, filter = "is_empty")
