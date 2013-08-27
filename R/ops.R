#' @include partial.R
NULL


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
"%|null|%" <- Partial(`%||%`, filter="is.null")


#' @export
"%|na|%" <- Partial(`%||%`, filter="is.na")


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

## http://stackoverflow.com/questions/7519790/assign-multiple-new-variables-in-a-single-line-in-r
#' @export
vassign <- function(..., values, envir=parent.frame()) {
  vars <- as.character(substitute(...()))
  values <- rep(values, length.out=length(vars))
  for(var in vars) {
    assign(var, values[[i]], envir)
  }
}
