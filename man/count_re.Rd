\name{count_re}
\alias{count_re}
\title{Count how often a pattern occurs in a character vector.}
\usage{
count_re(x, re, ...)
}
\arguments{
  \item{x}{Character vector.}

  \item{re}{Regular expression pattern passed to
  \code{gregexpr}.}

  \item{...}{Additional arguments passed to
  \code{\link{gregexpr}}.}
}
\description{
Count how often a pattern occurs in a character vector.
}
\examples{
count_re(c("foo", "bar", "baz"), "^b")
}

