#' Wrapper for system commands
#' 
#' @param exec The system command to be invoked.
#' @param ... Arguments passed on to the \code{system} command as name-value or 
#' name=\code{TRUE} pairs.
#' @param args Named list of arguments passed on to the \code{system} command.
#' Is merged with \code{...}.
#' @param stdin Input.
#' @param stdout Output.
#' @param redirection Redirection.
#' @param style One of \sQuote{unix} or \sQuote{gnu}.
#' @param sep Seperator of option and option argument.
#' @param show_cmd Have a look what the final command looks like.
#' @param intern Passed on to \code{\link{system}}'s \code{intern} argument.
#' @param input Passed on to \code{\link{system}}'s \code{input} argument.
#' 
#' @export
SysCall <- function (exec, ..., args = list(), stdin = NULL, stdout = NULL,
                     redirection = TRUE, style = c("unix", "gnu"), sep = " ",
                     show_cmd = FALSE, intern = FALSE, input = NULL) {  
  
  args <- merge_list(list(...), args)
  style <- match.arg(style)
  
  if (is.null(stdin)) {
    stdin <- ""
  } else if (!is.null(stdin) && redirection) {
    stdin <- paste("<", stdin)
  }
  
  if (is.null(stdout)) {
    stdout <- ""
  } else {
    stdout <- paste(">", stdout)
  }
  
  args[vapply(args, isTRUE, logical(1))] <- ""
  args[vapply(args, isFALSE, logical(1))] <- NULL
  args[vapply(args, is.null, logical(1))] <- NULL
  args <- switch(style,
                 unix=paste0(trim(sprintf("-%s%s%s", names(args), sep, args)), collapse=" "),
                 gnu=paste0(trim(sprintf("--%s%s%s", names(args), sep, args)), collapse=" "))
  
  if (show_cmd) {
    print(trim(paste(exec, args, stdin, stdout)))
  } else {
    return(system(trim(paste(exec, args, stdin, stdout)),
                  intern = intern, input = input) )
  }
}

