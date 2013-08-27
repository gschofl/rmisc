\name{Partial}
\alias{Curry}
\alias{Partial}
\title{Partial application (currying)}
\usage{
  Partial(fn, ..., .env = parent.frame())

  Curry(fn, ..., .env = parent.frame())
}
\arguments{
  \item{fn}{Function to apply partially.}

  \item{\dots}{Arguments that should be applied to
  \code{fn}}

  \item{.env}{the environment of the created function.
  Defaults to \code{\link{parent.frame}}.}
}
\description{
  Partial application (currying)
}

