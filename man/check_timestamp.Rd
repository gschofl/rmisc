\name{check_timestamp}
\alias{check_timestamp}
\title{Compare local and remote timestamp}
\usage{
  check_timestamp(url, file, .message)
}
\arguments{
  \item{url}{A string giving the URL.}

  \item{file}{The local file.}

  \item{.message}{A message}
}
\value{
  \code{FALSE} if the local file is more recent than the
  remote source, else \code{TRUE}
}
\description{
  Compare local and remote timestamp
}

