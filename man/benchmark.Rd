\name{benchmark}
\alias{benchmark}
\title{benchmark a function}
\usage{
  benchmark(FUN, n)
}
\arguments{
  \item{FUN}{A function call}

  \item{n}{Number of replicate runs}
}
\value{
  An object of class \code{\link{proc_time}}
}
\description{
  Call \code{\link{system.time}} n times and average over
  the replicate runs.
}

