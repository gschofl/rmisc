#' Generate a list flattener
#'
#' Generate a function that accepts an arbitrarily deeply nested \code{list}.
#' Use \code{flatten.at} to set the level of nestedness at which the flatterner
#' will start to flatten.
#'
#' @param flatten.at An \code{integer} specifying the layer after which to 
#' start the flattening. \code{1} means to start at the very top.
#' @return A function that can be used to flatten a nested list
#' @keywords internal
#' @export
make_flattener <- function(flatten.at = 1) {
  level <- 1
  .flatten <- function(x) {
    nm <- names(x)
    out <- list()
    for (i in seq_along(x)) {
      if (is.recursive(x[[i]])) {
        if (!is.null(nm[i])) {
          names(x[[i]]) <- paste0(nm[i], ".", names(x[[i]]))
        }
        level <<- level + 1
        out <- c(out, Recall(x[[i]]))
      } else {
        out <- c(out, x[i])
      }
    }
    level <<- level - 1
    out
  }
  
  function(x) {
    nm <- names(x)
    out <- list()
    for (i in seq_along(x)) {
      if (is.recursive(x[[i]]) && level >= flatten.at) {
        if (!is.null(nm[i])) {
          names(x[[i]]) <- paste0(nm[i], ".", names(x[[i]]))
        }
        level <<- level + 1
        out <- c(out, .flatten(x[[i]]))
      } else if (is.recursive(x[[i]]) && level < flatten.at) {
        level <<- level + 1
        out <- c(out, setNames(list(Recall(x[[i]])), nm[i]))
      } else {
        out <- c(out, x[i])
      }
    }
    if (level > 1) {
      level <<- level - 1
    }
    out
  }
}
