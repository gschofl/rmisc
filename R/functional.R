#' @include utils.R
NULL

#' Partial application (currying)
#' 
#' @param fn Function to apply partially.
#' @param \dots Arguments that should be applied to \code{fn}
#' @param .env the environment of the created function. Defaults to
#'   \code{\link{parent.frame}}.
#' @export
Partial <- function(fn, ..., .env = parent.frame()) {
  assert_that(is.function(fn))
  fcall <- substitute(fn(...))
  if (!is.primitive(fn))
    fcall <- match.call(fn, fcall)  
  fcall[[length(fcall) + 1]] <- quote(...)
  args <- list("..." = quote(expr = ))
  
  eval(call("function", as.pairlist(args), fcall), .env)
}


## deprecated
#' @rdname Partial
#' @export
Curry <- Partial


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
  fns <- lapply(list(...), match.fun)
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
  fns <- lapply(list(...), match.fun)
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
  fns <- lapply(list(...), match.fun)
  function(...) {
    value <- FALSE
    for (fn in fns)
      if (value <- fn(...))
        break
    value
  }
}


