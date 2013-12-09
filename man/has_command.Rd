\name{has_command}
\alias{has_command}
\title{Test if an external executable is available}
\usage{
has_command(cmd, msg = "")
}
\arguments{
  \item{cmd}{The exececutable to test for.}

  \item{msg}{Additional message if the test fails.}
}
\description{
Uses \code{\link{Sys.which}} internally, so it should work
on Windows and Unix.alikes.
}
\seealso{
Other tests: \code{\link{all_empty}},
\code{\link{are_empty}}, \code{\link{is.empty}};
\code{\link{all_scalar}}, \code{\link{are_scalar}},
\code{\link{is.scalar}}
}

