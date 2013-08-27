#' @include compact.R
NULL

#' Compose (multiple) functions
#'
#' Returns a function that applies the last argument to its input, than
#' the penultimate argument and so on.
#'
#' @param \dots The functions to be composed.
#' @param fn1,fn2 Two functions to compose (infix notaion)
#' @export
#' @examples
#'  x <- c(1,1,2,2,3,3)
#'  nunique <- Compose(length, unique)
#'  nunique(x) == length(unique(x))
#'  
#'  ## usefull in lapply constructs
#'  sapply(mtcars, length %.% unique)
Compose <- function (...) {
  fns <- lapply(compact(list(...)), match.fun)
  len <- length(fns)
  function (...) {
    res <- Call(fns[[len]], ...)
    for (fn in rev(fns[-len]))
      res <- fn(res)
    res
  }
}


## deprecated
#' @rdname Compose
#' @export
compose <- Compose


#' @rdname Compose
#' @export
"%.%" <- function(fn1, fn2) {
  fn1 <- match.fun(fn1)
  fn2 <- match.fun(fn2)
  function(...) fn1(fn2(...))
}


#' Compose functions
#'
#' Same as \code{\link{Compose}}, except that it applies the functions in
#' argument-list order.
#'
#' @param \dots The functions to be composed.
#' @export
Sequence <- function (...) {
  funs <- lapply(list(...), match.fun)
  function (...) {
    res <- Call(fns[[1]], ...)
    for (fn in fns[-1])
      res <- fn(res)
    res
  }
}
