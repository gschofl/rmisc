\name{is.empty}
\alias{all_empty}
\alias{are_empty}
\alias{is.empty}
\title{Is an object empty?}
\usage{
  is.empty(x)

  are_empty(x)

  all_empty(x)
}
\arguments{
  \item{x}{object to test}
}
\description{
  Which elements in a list are empty? Are all elements in a
  list of empty?
}
\examples{
is.empty(NULL)
is.empty(numeric())
is.empty(list())
is.empty("")
are_empty(list(1,NULL,3,NA))
all_empty(NULL)
all_empty(list(NULL, NULL, character()))
}
\seealso{
  Other tests: \code{\link{all_scalar}},
  \code{\link{are_scalar}}, \code{\link{has_command}},
  \code{\link{is.scalar}}
}

