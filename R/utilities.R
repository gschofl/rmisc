##' initialise a ProjectTemplate project
##'
##' use \code{init_project("path_to_project_root")} to generate
##' tags for the project and to load the project
##'
##' @param project_root path to the root directory of the project
##' @param src_dirs directories containing source files
##'
##' @export
init_project <- function (project_root=".",
                          src_dirs=c("diagnostics", 
                                     "lib",
                                     "munge",
                                     "profiling",
                                     "src",
                                     "tests")) {
  require('ProjectTemplate')
  setwd(project_root)
  if(!all(file.exists(file.path(getwd(), src_dirs))))
    stop("Not a valid project directory")
  src_path <- file.path(getwd(), src_dirs)
  tag_path <- file.path(src_path, "TAGS")
  rtags(path=src_path, recursive=TRUE, ofile="TAGS")
  file.remove(tag_path[file.exists(tag_path)])
  file.symlink(from=file.path(getwd(), "TAGS"), to=tag_path)
  load.project()
}

#' generate tag files
#'
#' generate tag files for use with the \emph{Vim-R-plugin}.
#' Depends on setting three \emph{options} in e.g. \file{.Rprofile}:
#' \itemize{
#'    \item \code{vim} path to your \file{~/.vim/} directory
#'    \item \code{packages} path to a directory containing all the R source code,
#'        e.g. \file{~/R/packages}
#'    \item \code{devel} path to your own development directory, e.g.
#'        \file{~/E/Devel}.
#'
#' generate_tag_files will look recursively for R code in the specified directories
#' and in the case of \code{packages} also for C, Fortran, Java and Tcl code
#' @usage \code{generate_tag_files()}
#' @export
generate_tag_files <- function() {
  stopifnot(require(stringr))
  print("Building Tags ...")
  unlink(c(str_c(getOption("vim"), "RTAGS"),
           str_c(getOption("vim"), "RsrcTags"),
           str_c(getOption("vim"), "RTAGS_DEVEL")))
  curr_dir <- getwd()                  # remember where we are
  setwd(getOption("packages"))
  rtags(path=getOption("packages"), recursive=TRUE, ofile="RTAGS")
  rtags(path=getOption("devel"), recursive=TRUE, ofile="RTAGS_DEVEL")
  file.symlink(str_c(getOption("packages"), "RTAGS"),
               str_c(getOption("vim"), "RTAGS"))
  file.symlink(str_c(getOption("packages"), "RTAGS_DEVEL"),
               str_c(getOption("vim"), "RTAGS_DEVEL"))
  system(paste("ctags --languages=C,Fortran,Java,Tcl -R -f RsrcTags",
               getOption("packages")))
  file.symlink(str_c(getOption("packages"), "RsrcTags"),
               str_c(getOption("vim"), "RsrcTags"))
  setwd(curr_dir)                      # get us back there ...
}

#' customised \code{\link[utils]{install.packages}}
#'
#' Puts source code in a specified \emph{packages} directory and
#' calls \code{\link{generate_tag_files}}
#'
#' @param pkgs character vector of package names
#' @param ... Arguments to be passed on to \code{\link[utils]{install.packages}}
#' @param Tags if \code{TRUE} call \code{\link{generate_tag_files}}
#' @export
install_packages <- function(pkgs, ..., Tags=TRUE) {
  stopifnot(require(foreach))
  curr_dir <- getwd()
  setwd(getOption("packages"))
  if (Tags == TRUE) {
    install.packages(pkgs, destdir=".", ...)
    files <- list.files(".", full.names=TRUE)
    tarballs <- files[grepl(".*tar\\.gz$", files)]
    dirs <- files[file.info(files)$isdir]
    newtars <- tarballs[!grepl(paste(basename(dirs), collapse="|"),
                               basename(tarballs))]
    if (length(newtars)) {
      foreach(f=iter(newtars)) %do% untar(f)
      generate_tag_files()
    }
  } else {
    install.packages(pkgs, ...)
  }
  setwd(curr_dir)
}

#' Install packages from Bioconductor
#'
#' Puts source code in a specified \emph{packages} directory and
#' calls \code{\link{generate_tag_files}}
#'
#' @param pkgs character vector of package names
#' @param Tags if \code{TRUE} call \code{\link{generate_tag_files}}
#' @seealso \code{\link{install_packages}}
#' @export
install_bioc <- function(pkg, Tags=TRUE) {
  source("http://bioconductor.org/biocLite.R")
  stopifnot(require(foreach))
  curr_dir <- getwd()
  setwd(getOption("packages"))
  if (Tags == TRUE) {
    biocLite(pkgs=pkg, lib="/usr/local/lib/R/site-library", destdir=".")
    files <- list.files(".", full.names=TRUE)
    tarballs <- files[grepl(".*tar\\.gz$", files)]
    dirs <- files[file.info(files)$isdir]
    newtars <- tarballs[!grepl(paste(basename(dirs), collapse="|"),
                               basename(tarballs))]
    if (length(newtars)) {
      foreach(f=iter(newtars)) %do% untar(f)
      generate_tag_files()
    }
  } else {
    biocLite(pkgs=pkg, lib="/usr/local/lib/R/site-library")
  }
  setwd(curr_dir)
}

#' plots a progress bar
#'
#' put it in a \code{for(i in seq(n))} loop
#'
#' @param i variable
#' @param n end point of loop
#' @param width width of bar
#' @param height height of bar
#' @param inv if \code{TRUE} the progress bar runs backwards
#' @export
progress_bar <- function(i, n, width=4, height=0.4, inv=FALSE) {
  stopifnot(require(grid))

  W <- i/n
  if (inv) {
    X <- 1 - W/2
    gp <- gpar(fill = "white", col = "white")
  } else {
    X <- W/2
    gp <- gpar(fill = "gray60", col = "white")
  }

  curdev <- dev.cur()
  if (!any(dev.list() == curdev)) {
    X11(w = width,h = height)
    if (inf) {
      grid.rect(x=0.5, w=1, gp=gpar(fill="gray60", col="white"))
    }
  }
  grid.rect(x = unit(X, "npc"), w = unit(W, "npc"), gp = gp)
  if(i == n) dev.off()
}

#' plot a progress bar backwords
#'
#' @param ... Arguments passed on to \code{progress_bar}
#' @seealso wrapper for \code{\link{progress_bar}}
#' @export
progress_bar_inv <- function(...) {
  progress_bar(..., inv=TRUE)
}

#' Strips extensions from file names
#'
#' @param file character vector of file names to be stripped
#' @export
strip_extension <- function(file) {
  sapply(file, function(x) strsplit(x, "\\.")[[1]][1])
}

#' Extract a match from a string or list of strings
#'
#' @param string a character vector
#' @param pattern a character string containing \link[=regex]{regular expressions}
#' @param ... things to pass on to \code{\link{gregexpr}}
#' @return returns a matrix of matches with multiple matches from each
#'    string in a row
#' @export
string_extract <- function(string, pattern, ...) {
  first <- gregexpr(pattern, string, ...)
  last <- lapply(first, function(x) x + attr(x, 'match.length') - 1)
  t(mapply(substring, string, first, last, USE.NAMES = FALSE))
}

#' list files recursively
#'
#' @param path character vector of path names
#' @param dir_pattern optional regexp
#' @param exclude_pattern optional regexp
#' @param file_pattern optional regexp
#' @export
list_files_rec <- function(path=".",
                           dir_pattern=NULL,
                           exclude_pattern=NULL,
                           file_pattern=NULL) {
  stopifnot(require(foreach))
  # find directories
  dirs <- list_dirs(path, dir_pattern, full.names=TRUE)
  # exclude some directories from the selection
  if(!is.null(exclude_pattern)) {
    dirs <- dirs[!grepl(exclude_pattern, dirs)]
  }
  # construct a list containing the full paths to files
  files <- foreach(dir = iter(dirs)) %do% {
    list.files(dir, file_pattern, full.names = TRUE)
  }
  # name the resulting list with the dir names
  names(files) <- basename(dirs)
  return(files)
}

#' list directories only
#'
#' @param path a character vector of path names
#' @param ... Arguments passed on to \code{\link{list.files}}
#' @export
list_dirs <- function(path, ...) {
    list.files(path, ...)[file.info(list.files(path, full.names=TRUE))$isdir]
}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
# --R-- vim:fdm=marker:fmr={{{,}}}:fdl=0:
