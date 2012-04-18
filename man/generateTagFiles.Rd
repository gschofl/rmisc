\name{generateTagFiles}
\alias{generateTagFiles}
\title{generate tag files}
\usage{
  generate_tag_files()
}
\description{
  generate tag files for use with the \emph{Vim-R-plugin}.
  Depends on setting three \emph{options} in
  \file{.Rprofile}:
}
\details{
  \itemize{ \item{\code{vim}}{path to your \file{~/.vim/}
  directory.} \item{\code{packages}}{path to a directory
  containing all the R source code, e.g.
  \file{~/R/packages}.} \item{\code{devel}}{path to your
  own development directory, e.g. \file{~/R/Devel}.} }

  \code{generate_tag_files} will look recursively for R
  code in the specified directories and in the case of
  \code{packages} also for C, Fortran, Java and Tcl code.
}

