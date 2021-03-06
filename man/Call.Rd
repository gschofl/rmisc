% Generated by roxygen2 (4.0.1): do not edit by hand
\name{Call}
\alias{Call}
\title{Call a function with arguments provided individually}
\usage{
Call(fn, ...)
}
\arguments{
\item{fn}{The function to call.}

\item{\dots}{Arguments to function \code{fn}.}
}
\description{
Call a function with arguments provided individually
}
\examples{
funs <- list("mean", "sd", "var")
sapply(funs, Call, 1:100)

## invoke an anonymous function
Call(function(a, b) a*b, 3, 4)
}

