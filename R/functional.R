#' @include compact.R
NULL

# Much of the code in here is taken from Hadley Wickham's
# "Advanced R Programming" (http://adv-r.had.co.nz/)

#' Partial function application.
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


#' Compose functions
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
Compose <- function(...) {
  fns <- lapply(compact(list(...)), match.fun)
  len <- length(fns)
  function(...) {
    res <- Call(fns[[len]], ...)
    for (fn in rev(fns[-len]))
      res <- fn(res)
    res
  }
}


#' @rdname Compose
#' @export
"%.%" <- function(g, f) {
  g <- match.fun(g)
  f <- match.fun(f)
  function(...) g(f(...))
}


#' Compose functions
#'
#' Same as \code{\link{Compose}}, except that it applies the functions in
#' argument-list order.
#'
#' @param \dots The functions to be composed.
#' @export
Sequence <- function(...) {
  fns <- lapply(list(...), match.fun)
  function(...) {
    res <- Call(fns[[1]], ...)
    for(fn in fns[-1])
      res <- fn(res)
    res
  }
}


#' Fast Map
#' 
#' A thin wrapper around internal \code{\link{mapply}}, which can be
#' a bit faster than base \code{\link{Map}}.
#' 
#' @param fn Function to apply
#' @param \dots Arguments to \code{fn}; Vectors or lists.
#' @return A list
#' @export
#' @examples
#' require(microbenchmark)
#' microbenchmark(
#' mapply(`*`, 1:100, 101:200),
#' Map(`*`, 1:100, 101:200),
#' FMap(`*`, 1:100, 101:200),
#' (1:100)*(101:200))
FMap <- function(fn, ...) {
  fn <- match.fun(fn)
  dots <- list(...)
  .mapply(fn, dots, MoreArgs = NULL)
}


#' Maybe call a function
#' 
#' If the argument(s) to a function are missing or \code{NULL},
#' return \code{NULL}; otherwise apply the function.
#' 
#' @param fn A function.
#' @return A function.
#' @export
Maybe <- function(fn) {
  function(x, ...) {
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
Fail_with <- function(default = NULL, fn, verbose = TRUE) {
  fn <- match.fun(fn)
  function(...) {
    out <- default
    tryCatch(out <- fn(...), error = function(e) {
      if (verbose) {
        message("Error caught: ", sQuote(e$message))
      } else {
        
      }
    })
    out
  }
}


#' Delay function call
#' 
#' @param delay delay in seconds.
#' @param fn function to call
#' @export
Delay_by <- function(delay, fn) {
  fn <- match.fun(fn)
  function(...) {
    Sys.sleep(delay)
    fn(...)
  }
}

#' Print a dot ever nth function call
#' 
#' @param n when to print a dot
#' @param fn function call
#' @export
Dot_every <- function(n, fn) {
  i <- 1
  fn <- match.fun(fn)
  function(...) {
    if (i %% n == 0) {
      cat('.')
    }
    i <<- i + 1
    fn(...)
  }
}


#' Log a time stamp and a message to file everytime a function is run
#' 
#' @param path path to log file
#' @param message logging message
#' @param fn function
#' @export
Log_to <- function(path, message="", fn) {
  fn <- match.fun(fn)
  now <- function(accuracy = 4) {
    paste0("-- ", format(Sys.time(), paste0("%M:%OS", accuracy)), " -- ")
  }
  if (missing(path) || is.null(path)) {
    function(...)
      fn(...)
  } else {
    assert_that(file.exists(path))
    function(...) {
      cat(now(), message, sep="", file=path, append=TRUE)
      fn(...)
    }
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


