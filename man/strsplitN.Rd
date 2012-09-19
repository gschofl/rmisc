\name{strsplitN}
\alias{strsplitN}
\title{Split up a string in pieces and return the nth piece.}
\usage{
  strsplitN(x, split, n = 3, from = c("start", "end"), ...)
}
\arguments{
  \item{x}{character vector to be split.}

  \item{split}{regular expression used for splitting.}

  \item{n}{piece to be returned after the split. Can be a
  vector.}

  \item{from}{One of \sQuote{start} or \sQuote{end}. From
  which direction do we count the pieces.}

  \item{...}{Arguments passed on to
  \code{\link{strsplit}}.}
}
\value{
  A character vector
}
\description{
  Split up a string in pieces and return the nth piece.
}

