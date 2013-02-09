#' Is empty?
#' 
#' @param x An object
#' @export
is_empty <- function (x) {#
  if (length(x) == 0)
    return(TRUE)
  !nzchar(x)
}


#' Not empty?
#' 
#' @usage not_empty(x)
#' @param x An object
#' @export
not_empty <- Negate(is_empty)


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
#' @param x A vector
#' @param tanble A vector
#' 
#' @export
"%ni%" <- Negate(`%in%`)


#' Not NA?
#' 
#' @usage x not.na(x)
#' @param x An R object
#' 
#' @export
not.na <- Negate(is.na)


#' Not NULL?
#' 
#' @usage x not.na(x)
#' @param x An R object
#' 
#' @export
not.null <- Negate(is.null)



