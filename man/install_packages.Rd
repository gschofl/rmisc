\name{install_packages}
\alias{install_packages}
\title{customised \code{\link[utils]{install.packages}}}
\usage{
  install_packages(pkgs, ..., Tags = TRUE)
}
\arguments{
  \item{pkgs}{character vector of package names}

  \item{...}{Arguments to be passed on to
  \code{\link[utils]{install.packages}}}

  \item{Tags}{if \code{TRUE} call
  \code{\link{generate_tag_files}}}
}
\description{
  Puts source code in a specified \emph{packages} directory
  and calls \code{\link{generate_tag_files}}
}

