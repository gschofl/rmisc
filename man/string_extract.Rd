\name{string_extract}
\alias{string_extract}
\title{Extract a match from a string or list of strings}
\usage{
  string_extract(string, pattern, ...)
}
\arguments{
  \item{string}{a character vector}

  \item{pattern}{a character string containing
  \link[=regex]{regular expressions}}

  \item{...}{things to pass on to \code{\link{gregexpr}}}
}
\value{
  returns a matrix of matches with multiple matches from
  each string in a row
}
\description{
  Extract a match from a string or list of strings
}

