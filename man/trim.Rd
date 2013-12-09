\name{trim}
\alias{trim}
\title{Trim elements}
\usage{
trim(x, trim = "\\\\s+")
}
\arguments{
  \item{x}{A vector.}

  \item{trim}{A pattern to trim from vector elements}
}
\value{
A vector with trimmed strings
}
\description{
Trim elements
}
\examples{
x <- " abc\\n\\t"
 trim(x)
}

