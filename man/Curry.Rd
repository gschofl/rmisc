\name{Curry}
\alias{Curry}
\title{Partial function application}
\usage{
  Curry(FUN, ..., .env = parent.frame())
}
\arguments{
  \item{FUN}{Function to curry.}

  \item{...}{Arguments that should be applied to
  \code{FUN}}

  \item{.env}{the environment of the created function.
  Defaults to \code{\link{parent.frame}}.}
}
\description{
  Partial function application
}

