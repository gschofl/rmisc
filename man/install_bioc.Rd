\name{install_bioc}
\alias{install_bioc}
\title{Install packages from Bioconductor}
\usage{
  install_bioc(pkg, Tags = TRUE)
}
\arguments{
  \item{pkgs}{character vector of package names}

  \item{Tags}{if \code{TRUE} call
  \code{\link{generate_tag_files}}}
}
\description{
  Puts source code in a specified \emph{packages} directory
  and calls \code{\link{generate_tag_files}}
}
\seealso{
  \code{\link{install_packages}}
}

