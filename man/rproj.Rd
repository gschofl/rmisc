\name{rproj}
\alias{rproj}
\title{Open an RStudio project from R}
\usage{
rproj(pkg, path = "all")
}
\arguments{
  \item{pkg}{The name of a package (or project directory)
  given as a Symbol.}

  \item{path}{One of 'all', 'pkgs', 'proj', or 'devel'.}
}
\value{
Opens RStudio.
}
\description{
Quickly open a package (ar any directory in the search
path) as an RStudio project. The search paths are defined
by the custom options \code{rmisc.pkgs}, \code{rmisc.proj},
and \code{rmisc.devel}, which I have preset in
\code{.Rprofile}. If none of these is defined \code{rproj}
will fall back to the current working directory.
}
\seealso{
Inspired by
\href{http://stackoverflow.com/questions/18426726/system-open-rstudio-close-connection}{this}
question on stackoverflow
}

