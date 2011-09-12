#' reset terminal width interactively
#' @param term.width
#' @export
set_width <- function(term.width=Sys.getenv("COLUMNS")) {
  options(width=term.width)
}

#' parallel assignment
#'
#' @param lhs left hand side of the operator
#' @param rhs right hand side of the operator
#' @rdname parallel_assignment
#' @note Pinched from \url{http://code.google.com/p/miscell/source/browse/rvalues/rvalues.r}
#'
#' usage: c(a,b,c) := c(1,2,3)
#'
#' @export
':=' <- function(lhs, rhs) {

  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1) {
    lhs <- lhs[-1]
  }
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula')) {
    rhs <- list(rhs)
  }
  if (length(lhs) > length(rhs)) {
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  }
  for (i in seq(length(lhs))) {
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  }
  return(invisible(NULL))
}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
# --R-- vim:fdm=marker:fmr={{{,}}}:fdl=0:


