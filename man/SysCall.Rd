\name{SysCall}
\alias{SysCall}
\title{Wrapper for system commands}
\usage{
  SysCall(exec, ..., args = list(), stdin = NULL,
    stdout = NULL, redirection = TRUE,
    style = c("unix", "gnu"), show_cmd = FALSE,
    intern = FALSE, input = NULL)
}
\arguments{
  \item{exec}{The system command to be invoked.}

  \item{...}{Arguments to the system command as name-value
  or name=\code{TRUE} pairs}

  \item{args}{Alternatively a named list of arguments}

  \item{stdin}{Where input should be fetched. A file or
  \code{NULL}.}

  \item{stdout}{Where output should be sent. A file or
  \code{NULL}.}

  \item{redirection}{Use redirection.}

  \item{style}{Unix-style \sQuote{\code{--}} or Gnu-style
  \sQuote{\code{-}} arguments.}

  \item{show_cmd}{Have a look what the final command looks
  like. Good for debugging.}

  \item{intern}{Passed on to \code{\link{system}}'s
  \code{intern} argument.}

  \item{input}{Passed on to \code{\link{system}}'s
  \code{input} argument.}
}
\description{
  Wrapper for system commands
}
\examples{
##
}

