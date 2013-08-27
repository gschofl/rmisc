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
