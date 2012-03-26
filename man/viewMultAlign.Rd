\name{viewMultAlign}
\alias{viewMultAlign}
\title{View sequence alignments in a web browser}
\usage{
  viewMultAlign(msa, start = 1L, end = width(msa)[1L],
    width = 80, browser = "google-chrome")
}
\arguments{
  \item{msa}{An XStringSet or MultipleAlignment object.}

  \item{start}{Start position of the alignment}

  \item{end}{End position of the alignment.}

  \item{width}{How many residues per line.}

  \item{browser}{Which web browser to use. It should be in
  the PATH, or a full path specified.}
}
\description{
  View sequence alignments in a web browser
}

