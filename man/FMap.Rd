\name{FMap}
\alias{FMap}
\title{Fast Map}
\usage{
  FMap(fn, ...)
}
\arguments{
  \item{fn}{Function to apply}

  \item{\dots}{Arguments to \code{fn}; Vectors or lists.}
}
\value{
  A list
}
\description{
  A thin wrapper around internal \code{\link{mapply}},
  which can be a bit faster than base \code{\link{Map}}.
}
\examples{
require(microbenchmark)
microbenchmark(
mapply(`*`, 1:100, 101:200),
Map(`*`, 1:100, 101:200),
FMap(`*`, 1:100, 101:200),
(1:100)*(101:200))
}

