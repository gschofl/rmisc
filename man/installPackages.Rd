\name{installPackages}
\alias{installPackages}
\title{customised \code{\link[utils]{install.packages}}}
\usage{
  installPackages(pkgs = "",
    repos = c("bioc", "CRAN", "Omegahat", "R-Forge", "github"),
    username = "gschofl", branch = "master", ...,
    tags = TRUE)
}
\arguments{
  \item{pkgs}{character vector of package names}

  \item{...}{Arguments to be passed on to
  \code{\link[utils]{install.packages}}}

  \item{check_updates}{if \code{TRUE} run
  \code{\link[utils]{old.packages}}}

  \item{tags}{if \code{TRUE} call
  \code{\link{generate_tag_files}}}

  \item{repos}{one of "CRAN", "bioc", "Omegahat", or
  "R-Forge". Defaults to "CRAN"}
}
\description{
  Puts source code in a specified \emph{packages} directory
  and calls \code{\link{generate_tag_files}}
}

