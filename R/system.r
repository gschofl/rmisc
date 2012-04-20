##' Wrapper for system commands
##' 
##' @param exec The system command to be invoked.
##' @param ... Options passed on to the system command as name-value or 
##' name=\code{TRUE} pairs.
##' @param opts Alternatively a named list of options.
##' @param args Arguments passed on to the system command.
##' @param params 'Parameters'. Options taking the form '-(-)option=value'.
##' @param stdin Input redirection. A file or \code{NULL}.
##' @param stdout Output redirection. A file or \code{NULL}.
##' @param stderr Error redirection. A file or \code{NULL}.
##' @param style \sQuote{\code{U}} for Unix-style \sQuote{\code{--}} or
##' \sQuote{\code{G}} for Gnu-style \sQuote{\code{-}} arguments.
##' Recyled over \code{...}, \code{opts}, \code{args}, and \code{params}.
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
                     opts = list(),
                     args = list(),
                     params = list(),
                     stdin = NULL,
                     stdout = NULL,
                     stderr = NULL,
                     style = "U",
                     show_cmd = FALSE,
                     intern = FALSE,
                     input = NULL)
{  
  isFALSE <- function (x) identical(FALSE, x)
  
  opts <- merge(list(...), opts)
  
  stdin <- if (is.null(stdin)) "" else paste("<", stdin)
  stdout <- if (is.null(stdout)) "" else paste(">", stdout)
  stderr <- if (is.null(stderr)) "" else paste("2>", stderr)
  
  opts[vapply(opts, isTRUE, logical(1))] <- ""
  opts[vapply(opts, isFALSE, logical(1))] <- NULL
  opts[vapply(opts, is.null, logical(1))] <- NULL
  
  is_param <- c(rep(FALSE, length(opts)), rep(TRUE, length(params)))
  opts_and_params <- merge(opts, params)
    
  if (length(style) > 1L && length(style) != length(opts_and_params))
    stop("The length of the style argument does not match the number of options")
  
  if (length(style) == 1L)
    style <- rep(style, length(opts_and_params))
  
  p1 <- "-"
  p2 <- ifelse(style == "U", "", "-")
  p3 <- names(opts_and_params)
  p4 <- ifelse(is_param, "=", " ")
  p5 <- opts_and_params
  
  opts_and_params <- 
    paste(str_trim(sprintf("%s%s%s%s%s", p1, p2, p3, p4, p5)), collapse=" ")

  args <- 
    paste(str_trim(sprintf("%s", args)), collapse=" ")
  
  if (show_cmd)
    print(str_trim(paste(exec, opts_and_params, args, stdin, stdout, stderr)))
  else
    return(system(str_trim(paste(exec, opts_and_params, args, stdin, stdout, stderr)),
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
##' @author Hadley Wickham <h.wickham@@gmail.com>
##' @export
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

#' @keywords internal
merge.list <- function (x, y, ...) 
{
  if (length(x) == 0) 
    return(y)
  if (length(y) == 0) 
    return(x)
  i = match(names(y), names(x))
  i = is.na(i)
  if (any(i)) 
    x[names(y)[which(i)]] = y[which(i)]
  x
}

