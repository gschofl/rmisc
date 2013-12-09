\name{plotInheritance}
\alias{plotInheritance}
\title{Plot the class inheritance graph}
\usage{
plotInheritance(classes, subclasses = FALSE, ...)
}
\arguments{
  \item{classes}{A character vector of class names.}

  \item{subclasses}{A logical flag. Include all known
  subclasses.}

  \item{\dots}{Further arguments.}
}
\value{
Plots a directed graph of class inheritance, and returns it
invisibly
}
\description{
This function is taken from the help page for
\code{\link[methods]{classesToAM}}.
}
\examples{
\dontrun{
plotInheritance(getClasses("package:Rentrez"))
}
}

