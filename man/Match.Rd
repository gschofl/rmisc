\name{Match}
\alias{Match}
\title{Perform a global regular expression match}
\usage{
  Match(pattern, str, i = NULL, perl = TRUE)
}
\arguments{
  \item{pattern}{a character string containing a regular
  expression.}

  \item{str}{a character vector where matches are sought.}

  \item{i}{(optional) number or name of capture group.}

  \item{perl}{if \code{TRUE} perl-compatible regexps are
  used}
}
\value{
  a list containing the matches the matches of the
  specifies capture group(s)
}
\description{
  Returns the match, a capture group, or an empty string if
  the match fails
}
\examples{
##
}

