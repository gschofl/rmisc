% Generated by roxygen2 (4.0.1): do not edit by hand
\name{compact}
\alias{compact}
\title{Filter \code{NULL} entries from a list.}
\usage{
compact(x)
}
\arguments{
\item{x}{A list.}
}
\description{
Filter \code{NULL} entries from a list.
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

