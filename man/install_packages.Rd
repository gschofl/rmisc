\name{install_packages}
\alias{install_packages}
\title{Package installer}
\usage{
install_packages(pkgs, destdir = getOption("rmisc.pkgs"), update = FALSE,
  ...)
}
\arguments{
  \item{pkgs}{Packages}

  \item{destdir}{Where to put the source code.}

  \item{update}{call \code{\link{update_packages}}.}

  \item{...}{Further arguments passed on to
  \code{\link{biocLite}}.}
}
\description{
Wrapper around \code{\link{biocLite}}. In addition to the
standard functionality provided \code{biocLite}, this
function also download and untars the source files to
\code{destdir}.
}

