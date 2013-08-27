\name{compact}
\alias{compact}
\title{Filter entries from a list.}
\usage{
  compact(x, filter = "is.null")
}
\arguments{
  \item{x}{A list.}

  \item{filter}{A function to filter entries. (default
  \code{is.null})}
}
\description{
  Filter entries from a list.
}
\examples{
l <- list(a=1, b=NULL, c=NA)
compact(l)
## $a
## [1] 1
##
## $c
## [1] NA
}

