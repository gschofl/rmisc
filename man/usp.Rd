\name{usp}
\alias{usp}
\title{unlist(strsplit(x, split, ...))}
\usage{
  usp(x, split, ...)
}
\arguments{
  \item{x}{Character vector to be split.}

  \item{split}{The regexp to split on.}

  \item{\dots}{Arguments passed on to
  \code{\link{strsplit}}.}
}
\description{
  unlist(strsplit(x, split, ...))
}
\examples{
usp("a.b.c", ".", fixed = TRUE)
}

