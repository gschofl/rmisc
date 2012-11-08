\name{benchmark}
\alias{benchmark}
\title{Benchmark a function}
\usage{
  benchmark(FUN, n = 1)
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

