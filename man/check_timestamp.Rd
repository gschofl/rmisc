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
  \code{TRUE} if the remote source is more recent than the
  local file, or if the local file does not exist, else
  \code{FALSE}
}
\description{
  Compare local and remote timestamp
}

