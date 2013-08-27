#' @include compact.R
NULL

#' Maybe call a function
#' 
#' If the argument(s) to a function are missing or \code{NULL},
#' return \code{NULL}; otherwise apply the function.
#' 
#' @param fn A function.
#' @return A function.
#' @export
Maybe <- function (fn) {
  function (x, ...) {
    if (missing(x) || is.null(x))
      return(NULL)
    fn(x, ...)
  }
}


#' Fail with a default value
#' 
#' @param default Default value.
#' @param fn A function
#' @param verbose Show error message
#' 
#' @export
Failwith <- function(default = NULL, fn, verbose = TRUE) {
  function (...) {
    out <- default
    tryCatch(out <- fn(...), error = function (e) {
      if (verbose) {
        message("Error caught: ", sQuote(e$message))
      } else {
        
      }
    })
    out
  }
}


#' Call a function with arguments provided individually
#' 
#' @param fn The function to call.
#' @param \dots Arguments to function \code{fn}.
#' 
#' @export
#' @examples
#' funs <- list("mean", "sd", "var")
#' sapply(funs, Call, 1:100)
#'
#' ## invoke an anonymous function
#' Call(function(a, b) a*b, 3, 4)
Call <- function(fn, ...) {
  fn <- match.fun(fn)
  fn(...)
}


#' Predicates
#' 
#' @details
#' \code{And} returns a function that returns \code{TRUE} when all the arguments,
#' applied to the returned function's arguments, returns \code{TRUE}.
#' 
#' \code{Or} returns a function that returns \code{TRUE} when any the arguments,
#' applied to the returned function's arguments, returns \code{TRUE}.
#' 
#' @param \dots Predicate functions
#' @rdname Predicates
#' @export
And <- function(...) {
  fns <- lapply(compact(list(...)), match.fun)
  function(...) {
    value <- TRUE
    for (fn in fns)
      if (!(value <- fn(...)))
        break
    value
  }
}


#' @param \dots Predicate functions
#' @rdname Predicates
#' @export
Or <- function(...) {
  fns <- lapply(compact(list(...)), match.fun)
  function(...) {
    value <- FALSE
    for (fn in fns)
      if (value <- fn(...))
        break
    value
  }
}


