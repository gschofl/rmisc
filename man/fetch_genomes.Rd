\name{fetch_genomes}
\alias{fetch_genomes}
\title{Retrieve genomes from a remote database}
\usage{
  fetch_genomes(db = "NCBI", dest = ".",
    which_files = "gbk$", which_dirs = "",
    keep_dirs = NULL, update_dirs = TRUE,
    update_files = TRUE, start_from_scratch = FALSE,
    SORT = FALSE, VERBOSE = FALSE)
}
\arguments{
  \item{db}{which remote database. Currently only NCBI}

  \item{dest}{download directory}

  \item{which_files}{regular expression matching the
  extensions of the files to be downloded. E.g.
  "gff$|fna$"}

  \item{which_dirs}{regular expression matching the genome
  directories to download from}

  \item{keep_dirs}{regular expression matching local
  directories that shouldn't be touched}

  \item{update_dirs}{if \code{TRUE} new directories
  matching \code{which_dirs} will be downloaded}

  \item{update_files}{if \code{TRUE} files in locally
  existing dirctories will be updated if newer versions
  exist in the remote database}

  \item{start_from_scratch}{}

  \item{SORT}{}

  \item{VERBOSE}{}
}
\description{
  works currently with bacterial genomes from NCBI
}

