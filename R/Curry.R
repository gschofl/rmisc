#' Partial function application 
#' 
#' @param FUN Function to curry.
#' @param ... Arguments that should be applied to \code{FUN}
#' @param .env the environment of the created function. Defaults to
#'   \code{\link{parent.frame}}.
#' @export
Curry <- function(FUN, ..., .env = parent.frame()) {
  assert_that(is.function(FUN))
  Fcall <- substitute(FUN(...))
  if (!is.primitive(FUN))
   Fcall <- match.call(FUN, Fcall)
  Fcall[[length(Fcall) + 1]] <- quote(...)
  args <- list("..." = quote(expr = ))
  
  eval(call("function", as.pairlist(args), Fcall), .env)
}

