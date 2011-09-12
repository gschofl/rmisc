#' Create or update a package
#'
#' This function creates, documents, loads or installs a package
#' from scratch or adds code and documentation to an existing package
#'
#' @param pkg name of a package or path to a package.
#' @param code_files a character vector with paths to source
#'    code files. See Details for the Defaults.
#' @param document if \code{true} the documentation is updated by a call to
#'    \code{\link[devtools]{document}}.
#' @param load if \code{true} the package will be loaded by a call to
#'    \code{\link[devtools]{load_all}}.
#' @param install if \code{true} the package will be installed
#' @param clean if \code{true} the source files will be removed after
#'    copying to the package directory. Existing target files in the package
#'    directory are overwritten.
#'
#' @details If the provided name or path doesn't point to an existing
#'    package or directory a new package will be created by a call to
#'    \code{\link{package.skeleton}}. An existing package will be updated.
#'
#'    If no code files are specified, \code{repackage} will attempt
#'    to find code files to use.
#'
#'    If a path to a package is provided and a directory of that name exists,
#'    \file{.R} files residing in that directory are passed on to
#'    \code{\link{package.skeleton}}. If the directory does not exist,
#'    \file{.R} files from its parent directory are used.
#'
#'    If a package name is provided and a directory of that name exists
#'    somewhere in the path to the current working directory, the \file{R}
#'    files in the current working directory are used to create a package in
#'    that directory. If a directory of that name does not exist, file{.R}
#'    files from the current working directory are used and a new package 
#'    is created.
#'
#'    If the package exists (complete with a \file{DESCRIPTION} file and a
#'    \file{myPackage/R} subdirectory), source files from the package #'     
#'    directory or the current working directory will be added to the package 
#'    and the documentation will be updated.
#'
#'    Which directory will be used depends on whether a path to the package
#'    or the package name were provided. Note that a package can be called
#'    by name from any working directory within the package directory
#'    (adding then source files from within that directory to the package).
#'
#' @export
repackage <- function(pkg="myPackage", code_files=NULL, document=TRUE,
                      load=TRUE, install=FALSE, clean=FALSE) {
  require(stringr)
  require(roxygen2)
  require(devtools)
  if (missing(pkg)) stop("Provide a path to the package")
  is_dir <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
  is_path <- function(x) grepl(.Platform$file.sep, pkg, fixed=TRUE)

  if (is_path(pkg)) {
    par_dir <- dirname(pkg)
    pack_dir <- pkg
    pkg <- basename(pkg)
    r_dir <- file.path(pack_dir, "R")
    desc_file <- file.path(pack_dir, "DESCRIPTION")
    # collect R files from the package directory if it exists, alternatively
    # try the parental directory. Used only if no sources files are provided 
    r_files <- list.files(path=ifelse(is_dir(pack_dir), pack_dir, par_dir),
                          pattern=".*\\.[rR]$", full.names=TRUE)
    has_pkg <- ifelse(is_dir(r_dir) && file.exists(desc_file), TRUE, FALSE)
  } else {
    split_path <- unlist(str_split(getwd(), .Platform$file.sep))
    if (any(hit <- grepl(pattern=str_c("^", pkg, "$"), split_path))) {
      par_dir <- str_c(split_path[seq(which(hit) - 1)],
                       collapse=.Platform$file.sep)
    } else {
      par_dir <- getwd()
    }
    pack_dir <- file.path(par_dir, pkg)
    r_dir <- file.path(pack_dir, "R")
    desc_file <- file.path(pack_dir, "DESCRIPTION")
    # collect R files from the current directory itself if a path was
    # provided.
    r_files <- list.files(path=getwd(), pattern=".*\\.[rR]$", full.names=TRUE)
    has_pkg <- ifelse(!is_dir(r_dir) && !file.exists(desc_file), FALSE, TRUE)
  }

  if (has_pkg) {
    # DESCRIPTION and /myPackage/R exist
    sources_new <-c(if (!is.null(code_files)) {code_files}, r_files)
    sources <- c(sources_new, list.files(path=r_dir, pattern=".*\\.[rR]$",
                                         full.names=TRUE))
    message("Going to repackage ", pkg, " using files:")
    message(str_c("\t", basename(sources), collapse="\n"))
    if (length(sources_new) > 0) { 
      message("newly added files:")
      message(str_c("\t", basename(sources_new), collapse="\n"))
    }
    if (clean) {
      file.copy(from=sources_new, to=r_dir, overwrite=TRUE)
      unlink(sources_new)
    } else {
      file.copy(from=sources_new, to=r_dir, overwrite=FALSE)
    }
    if (document) document(pkg=pack_dir, clean=TRUE)
    if (load && !install) load_all(pkg=pack_dir)
  } else if (!has_pkg) {
    # DESCRIPTION and /myPackage/R don't exist
    if (is_dir(pack_dir)) {
      # but a directory with the same name as the package exists
      if (is.null(code_files)) sources <- r_files else sources <- code_files
      if (length(sources) == 0) stop("No source code to work with")
      # warn that the package directory already exists and that you
      # are going to deploy the package in it
      warning(str_c("Going to create package '", pkg, "' in ", par_dir ))
      package.skeleton(name=pkg, code_files=sources, path=par_dir,
                       namespace=TRUE, force=TRUE)
      if (clean) unlink(sources)
      if (document) document(pkg=pack_dir)
      if (load && !install) load_all(pkg=pack_dir)
    } else {
      # no package exists - run package.skeleton
      if (is.null(code_files)) sources <- r_files else sources <- code_files
      if (length(sources) == 0) stop("No source code to work with")
      package.skeleton(name=pkg, code_files=sources, path=par_dir,
                       namespace=TRUE, force=FALSE)
      if (clean) unlink(sources)
      if (document) document(pkg=pack_dir)
      if (load && !install) load_all(pkg=pack_dir)
    }
  }
  
  if (install) {
    check_doc(pkg=pack_dir)
    build(pkg=pack_dir, path=par_dir)
    install(pkg=pack_dir)
  }
  return(invisible(NULL))
}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
# --R-- vim:fdm=marker:fmr={{{,}}}:fdl=0:
