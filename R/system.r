##' Wrapper for system commands
##' 
##' @param exec The system command to be invoked.
##' @param ... Arguments to the system command as name-value or 
##' name=\code{TRUE} pairs
##' @param args Alternatively a named list of arguments
##' @param stdin Where input should be fetched. A file or \code{NULL}.
##' @param stdout Where output should be sent. A file or \code{NULL}.
##' @param redirection Use redirection.
##' @param style Unix-style \sQuote{\code{--}} or Gnu-style 
##' \sQuote{\code{-}} arguments.
##' @param show_cmd Have a look what the final command looks like. Good for
##' debugging.
##' @param intern Passed on to \code{\link{system}}'s \code{intern} argument.
##' @param input Passed on to \code{\link{system}}'s \code{input} argument.
##' 
##' @export
##' @examples
##' ##
SysCall <- function (exec,
                     ...,
                     args = list(),
                     stdin = NULL,
                     stdout = NULL,
                     redirection = TRUE,
                     style = c("unix", "gnu"),
                     show_cmd = FALSE,
                     intern = FALSE,
                     input = NULL)
{  
  isFALSE <- function (x) identical(FALSE, x)
  
  args <- merge(list(...), args)
  style <- match.arg(style)
  
  if (is.null(stdin)) {
    stdin <- ""
  }
  else if (!is.null(stdin) && redirection) {
    stdin <- paste("<", stdin)
  }
  
  if (is.null(stdout)) {
    stdout <- ""
  }
  else {
    stdout <- paste(">", stdout)
  }
  
  args[vapply(args, isTRUE, logical(1))] <- ""
  args[vapply(args, isFALSE, logical(1))] <- NULL
  args[vapply(args, is.null, logical(1))] <- NULL
  args <- switch(style,
                 unix=paste(str_trim(sprintf("-%s %s", names(args), args)),
                            collapse=" "),
                 gnu=paste(str_trim(sprintf("--%s %s", names(args), args)),
                           collapse=" ")
                 )
  if (show_cmd)
    print(str_trim(paste(exec, args, stdin, stdout)))
  else
    return(system(str_trim(paste(exec, args, stdin, stdout)),
                  intern = intern, input = input))
}


##' Curry a function
##' 
##' Pinched from 
##' \href{https://github.com/hadley/devtools/wiki/First-class-functions}{here}.
##' 
##' @param FUN Function to curry.
##' @param ... Arguments
##' 
##' @export
##' @examples
##' ##
Curry <- function (FUN, ...)
{
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