\name{init_project}
\alias{init_project}
\title{initialise a ProjectTemplate project}
\usage{
  init_project(project_root = ".", src_dirs =
  c("diagnostics", "lib", "munge", "profiling", "src",
  "tests"))
}
\arguments{
  \item{project_root}{path to the root directory of the
  project}

  \item{src_dirs}{directories containing source files}
}
\description{
  use \code{init_project("path_to_project_root")} to
  generate tags for the project and to load the project
}

