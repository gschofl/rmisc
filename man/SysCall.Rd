\name{SysCall}
\alias{SysCall}
\title{Wrapper for system commands}
\usage{
  SysCall(exec, ..., opts = list(), args = list(),
    params = list(), stdin = NULL, stdout = NULL,
    stderr = NULL, style = "U", show_cmd = FALSE,
    intern = FALSE, input = NULL)
}
\arguments{
  \item{exec}{The system command to be invoked.}

  \item{...}{Options passed on to the system command as
  name-value or name=\code{TRUE} pairs.}

  \item{opts}{Alternatively a named list of options.}

  \item{args}{Arguments passed on to the system command.}

  \item{params}{'Parameters'. Options taking the form
  '-(-)option=value'.}

  \item{stdin}{Input redirection. A file or \code{NULL}.}

  \item{stdout}{Output redirection. A file or \code{NULL}.}

  \item{stderr}{Error redirection. A file or \code{NULL}.}

  \item{style}{\sQuote{\code{U}} for Unix-style
  \sQuote{\code{--}} or \sQuote{\code{G}} for Gnu-style
  \sQuote{\code{-}} arguments. Recyled over \code{...},
  \code{opts}, \code{args}, and \code{params}.}

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

