\name{strsplitN}
\alias{strsplitN}
\title{Split up a string in pieces and return the nth piece.}
\usage{
  strsplitN(x, split, n, from = "start", collapse = split,
    ...)
}
\arguments{
  \item{x}{character vector to be split.}

  \item{split}{regular expression used for splitting.}

  \item{n}{Vector of elements to be returned.}

  \item{from}{One of \sQuote{start} or \sQuote{end}. From
  which direction do we count the pieces.}

  \item{collapse}{}

  \item{...}{Arguments passed on to
  \code{\link{strsplit}}.}
}
\value{
  A character vector.
}
\description{
  Split up a string in pieces and return the nth piece.
}
\examples{
str <- c("Lorem, ipsum, dolor", "consectetur, adipisicing, elit")
strsplitN(str, ", ", 1)
## [1] "Lorem"
## [2] "consectetur"
strsplitN(str, ", ", 1, from="end")
## [1] "dolor"
## [2] "elit"
strsplitN(str, ", ", 1:2)
## [1] "Lorem, ipsum"
## [2] "consectetur, adipisicing"
strsplitN(str, ", ", 1:2, collapse = "--")
## [1] "Lorem--ipsum"
## [2] "consectetur--adipisicing"
}

