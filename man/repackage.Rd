\name{repackage}
\alias{repackage}
\title{Create or update a package}
\usage{
  repackage(pkg = "myPackage", code_files = NULL,
    document = TRUE, load = TRUE, install = FALSE,
    clean = FALSE)
}
\arguments{
  \item{pkg}{name of a package or path to a package.}

  \item{code_files}{a character vector with paths to source
  code files. See Details for the Defaults.}

  \item{document}{if \code{true} the documentation is
  updated by a call to \code{\link[devtools]{document}}.}

  \item{load}{if \code{true} the package will be loaded by
  a call to \code{\link[devtools]{load_all}}.}

  \item{install}{if \code{true} the package will be
  installed}

  \item{clean}{if \code{true} the source files will be
  removed after copying to the package directory. Existing
  target files in the package directory are overwritten.}
}
\description{
  This function creates, documents, loads or installs a
  package from scratch or adds code and documentation to an
  existing package
}
\details{
  If the provided name or path doesn't point to an existing
  package or directory a new package will be created by a
  call to \code{\link{package.skeleton}}. An existing
  package will be updated.

  If no code files are specified, \code{repackage} will
  attempt to find code files to use.

  If a path to a package is provided and a directory of
  that name exists, \file{.R} files residing in that
  directory are passed on to
  \code{\link{package.skeleton}}. If the directory does not
  exist, \file{.R} files from its parent directory are
  used.

  If a package name is provided and a directory of that
  name exists somewhere in the path to the current working
  directory, the \file{R} files in the current working
  directory are used to create a package in that directory.
  If a directory of that name does not exist, file{.R}
  files from the current working directory are used and a
  new package is created.

  If the package exists (complete with a \file{DESCRIPTION}
  file and a \file{myPackage/R} subdirectory), source files
  from the package directory or the current working
  directory will be added to the package and the
  documentation will be updated.

  Which directory will be used depends on whether a path to
  the package or the package name were provided. Note that
  a package can be called by name from any working
  directory within the package directory (adding then
  source files from within that directory to the package).
}

