\name{install_packages}
\alias{install_packages}
\title{customised \code{\link[utils]{install.packages}}}
\usage{
  install_packages(pkgs, ..., tags = TRUE, bioc = FALSE)
}
\arguments{
  \item{pkgs}{character vector of package names}

  \item{...}{Arguments to be passed on to
  \code{\link[utils]{install.packages}}}

  \item{tags}{if \code{TRUE} call
  \code{\link{generate_tag_files}}}

  \item{bioc}{if \code{TRUE} install from Bioconductor}
}
\description{
  Puts source code in a specified \emph{packages} directory
  and calls \code{\link{generate_tag_files}}
}

