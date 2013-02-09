#' @include ops.R
NULL


#' Is empty?
#' 
#' @param x A vector.
#' @export
is_empty <- function (x) {
  if (is.null(x) || length(x) == 0)
    return(TRUE)
  else
    vapply(x, function(x) length(x)==0, logical(1), USE.NAMES=FALSE) | !nzchar(x)
}


#' Not empty?
#' 
#' @usage not_empty(x)
#' @param x An object
#' @export
not_empty <- Negate(is_empty)


#' All empty?
#' 
#' @usage all_empty(x)
#' @param x An object
#' @export
all_empty <- compose(all, is_empty)


#' Any empty?
#' 
#' @usage any_empty(x)
#' @param x An object
#' @export
any_empty <- compose(any, is_empty)


#' Is FALSE?
#' 
#' @usage isFALSE(x) 
#' @param x An object
#' 
#' @export
isFALSE <- function (x) {
  identical(FALSE, x)
}


#' Not in?
#' 
#' @usage x %ni% table
#' @param x A vector.
#' @param table A vector.
#' @export
"%ni%" <- Negate(`%in%`)


#' Not NA?
#' 
#' @usage x not.na(x)
#' @param x An R object
#' @export
not.na <- Negate(is.na)


#' Not NULL?
#' 
#' @usage x not.na(x)
#' @param x An R object
#' 
#' @export
not.null <- Negate(is.null)


#' Is NULL?
#' 
#' Like \code{\link{is.null}} but tests also for
#' \code{NULL} entries in lists
#' 
#' @param x An R object
#' @export
is_null <- function(x) {
  if (is.null(x))
    return(TRUE)
  else if (length(x) == 0)
    return(FALSE)
  else 
    vapply(x, is.null, logical(1), USE.NAMES=FALSE)
}


#' Not NULL?
#' 
#' Like \code{\link{not.null}} but tests also for
#' \code{NULL} entries in lists
#' 
#' @usage x not_null(x)
#' @param x An R object
#' 
#' @export
not_null <- Negate(is_null)

