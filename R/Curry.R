#' Curry a function
#' 
#' Pinched from 
#' \href{https://github.com/hadley/devtools/wiki/First-class-functions}{here}.
#' 
#' @param FUN Function to curry.
#' @param ... Arguments
#' 
#' @author Hadley Wickham <h.wickham@@gmail.com>
#' @export
Curry <- function (FUN, ...) {
  args <- match.call(expand.dots=FALSE)$...
  args$... <- as.name("...")
  
  env <- new.env(parent=parent.frame())
  
  if (is.name(FUN)) {
    fname <- FUN
  } else if (is.character(FUN)) {
    fname <- as.name(FUN)
  } else if (is.function(FUN)) {
    fname <- as.name("FUN")
    env$FUN <- FUN
  } else {
    stop("FUN not function or name of function")
  }
  
  curry_call <- as.call(c(list(fname), args))
  
  f <- eval(call("function", as.pairlist(alist(... = )), curry_call))
  environment(f) <- env
  f
}

