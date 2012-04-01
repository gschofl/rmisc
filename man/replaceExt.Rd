\name{replaceExt}
\alias{replaceExt}
\title{Replace file extensions}
\usage{
  replaceExt(file, replacement = "", sep = "\\.",
    level = 0)
}
\arguments{
  \item{replacement}{replacement extension}

  \item{file}{file name(s)}

  \item{sep}{specifies the seperator character (default
  ".").}

  \item{level}{numeric denoting how many extensions should
  be stripped. The default (0) strips all, 1 strips the
  last one, 2 strips the last two, and so on.}
}
\description{
  Replace file extensions
}

