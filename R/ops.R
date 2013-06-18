## Chain functions
#' @export
"%@%" <- function(x, f) {
  eval.parent(as.call(append(as.list(substitute(f)), list(x), 1)))
}


#' @export
"%||%" <- function (a, b, filter="is.empty") {
  filter <- match.fun(filter)
  if (filter(a)) b else a
}

## Vectorized version of %||%
#' @export
"%|%" <- function (a, b, filter="are_empty") {
  filter <- match.fun(filter)
  ifelse(filter(a), b, a)
}


#' @export
"%|null|%" <- Curry("%||%", filter="is.null")


#' @export
"%|na|%" <- Curry("%||%", filter="is.na")


## Compose functions
#' @export
compose <- function(f, g) {
  f <- match.fun(f)
  g <- match.fun(g)
  function(...) f(g(...))
}


#' @export
"%.%" <- compose


# Pinched from http://code.google.com/p/miscell/source/browse/rvalues/rvalues.r
#' @export
":=" <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1) {
    lhs <- lhs[-1]
  }
  if (length(lhs) == 1) {
    assign(as.character(lhs[[1]]), rhs, envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula')) {
    rhs <- list(rhs)
  }
  if (length(lhs) > length(rhs)) {
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  }
  for (i in seq_along(lhs)) {
    assign(as.character(lhs[[i]]), rhs[[i]], envir=frame)
  }
  return(invisible(NULL))
}

