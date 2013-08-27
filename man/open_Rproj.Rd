\name{open_Rproj}
\alias{open_Rproj}
\title{Open an RStudio project from R}
\usage{
  open_Rproj(pkg, path = "all")
}
\arguments{
  \item{pkg}{The name of a package (or project directory)
  given as a Symbol.}

  \item{path}{One of 'all', 'packages', 'projects', or
  'devel'.}
}
\value{
  Opens RStudio.
}
\description{
  Quickly open a package (ar any directory in the search
  path) as an RStudio project. The search paths are defined
  by the custom options \code{packages}, \code{projects},
  and \code{devel}, which I have preset in
  \code{.Rprofile}.
}
\seealso{
  Inspired by
  \href{http://stackoverflow.com/questions/18426726/system-open-rstudio-close-connection}{this}
  question on stackoverflow
}

