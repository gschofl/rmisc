##' initialise a ProjectTemplate project
##'
##' use \code{init_project("path_to_project_root")} to generate
##' tags for the project and to load the project
##'
##' @param project_root path to the root directory of the project
##' @param src_dirs directories containing source files
##'
##' @export
initProject <- function (project_root=".",
                         src_dirs=c("diagnostics", 
                                    "lib",
                                    "munge",
                                    "profiling",
                                    "src",
                                    "tests")) {
  
  stopifnot(require(ProjectTemplate))
  
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

##' generate tag files
##'
##' generate tag files for use with the \emph{Vim-R-plugin}.
##' Depends on setting three \emph{options} in \file{.Rprofile}:
##' \itemize{
##'    \item \code{vim} path to your \file{~/.vim/} directory
##'    \item \code{packages} path to a directory containing all the R source code,
##'        e.g. \file{~/R/packages}
##'    \item \code{devel} path to your own development directory, e.g.
##'        \file{~/R/Devel}.
##'
##' generate_tag_files will look recursively for R code in the specified directories
##' and in the case of \code{packages} also for C, Fortran, Java and Tcl code
##' @usage \code{generate_tag_files()}
##' @export
generateTagFiles <- function() {
  message("Building Tags ...")
  unlink(paste0(options()$vim, c("RTAGS","RsrcTags")))
  curr_dir <- getwd()                  # remember where we are
  setwd(options()$packages)

  rtags(path=options()$devel, recursive=TRUE, ofile="RTAGS")
  rtags(path=options()$packages, recursive=TRUE, ofile="RTAGS", append=TRUE)
  rtags(path=options()$base, recursive=TRUE, ofile="RTAGS", append=TRUE)

  system(paste("/usr/bin/ctags --languages=C,C++,Fortran -R -f RsrcTags", options()$devel))
  system(paste("/usr/bin/ctags --languages=C,C++,Fortran -R -a -f RsrcTags", options()$packages))
  system(paste("/usr/bin/ctags --languages=C,C++,Fortran -R -a -f RsrcTags", options()$base))

  file.symlink(paste0(options()$packages, "RTAGS"), paste0(options()$vim, "RTAGS"))
  file.symlink(paste0(options()$packages, "RsrcTags"), paste0(options()$vim, "RsrcTags"))

  setwd(curr_dir)                      # get us back there ...
}

##' customised \code{\link[utils]{install.packages}}
##'
##' Puts source code in a specified \emph{packages} directory and
##' calls \code{\link{generate_tag_files}}
##'
##' @param pkgs character vector of package names
##' @param ... Arguments to be passed on to \code{\link[utils]{install.packages}}
##' @param check_updates if \code{TRUE} run \code{\link[utils]{old.packages}}
##' @param tags if \code{TRUE} call \code{\link{generate_tag_files}}
##' @param repos one of "CRAN", "bioc", "Omegahat", or "R-Forge".
##' Defaults to "CRAN"
##'
##' @export
installPackages <- function(pkgs, ..., 
                            check_updates=FALSE,
                            tags=TRUE,
                            repos="bioc") {

  stopifnot(require(foreach))
  stopifnot(require(BiocInstaller))
  
  ops <- options("repos")
  setRepositories(ind=1:20)
  all_repos <- getOption("repos")
  on.exit(options(ops))
  select_repos <- c(bioc="bioc", all_repos[c("CRAN", "Omegahat", "R-Forge")])

  if (is.na(select_repos[repos]))
    stop("Selected Repository not available")

  switch(repos,
         bioc = options(repos=biocinstallRepos()),
         options(repos=select_repos[repos]))

  if (check_updates) {
     old.packages()
  } else {
    curr_dir <- getwd()
    setwd(getOption("packages"))
    if (isTRUE(tags)) {
      ## if the package is present in the packages directory delete
      ## it and do a fresh install
      rm_files <- list.files(".", full.names=TRUE)
      rm_files <- rm_files[grepl(paste(pkgs, collapse="|"), rm_files)]
      unlink(rm_files, recursive=TRUE)
      ## install from CRAN or Bioconductor
      if (repos == "bioc") {
        biocLite(pkgs=pkgs, destdir=".", suppressUpdates=TRUE, ...)
      } else {
        install.packages(pkgs, destdir=".", ...)
      }
      ## expand tarballs in destdir and generate tags
      all_files <- list.files(".", full.names=TRUE)
      tar_files <- all_files[grepl(".*tar\\.gz$", all_files)]
      dirs <- all_files[file.info(all_files)$isdir]
      expand_files <- tar_files[!grepl(paste(gsub("^\\./", "", dirs),
                                             collapse="|"), tar_files)]

      if (length(expand_files)) {
        foreach(f=iter(expand_files)) %do% untar(f)
        generateTagFiles()
      }
    } else {
      if (repos == "bioc") {
        biocLite(pkgs=pkgs, ...)
      } else {
        install.packages(pkgs, ...)
      }
    }
    setwd(curr_dir)
    }
}


#' Strips extensions from file names
#'
#' @param file character vector of file names to be stripped
#' @export
stripExtension <- function(file) {
  base <- vapply(file, function(x) strsplit(x, "\\.")[[1L]][1L], FUN.VALUE=character(1))
  names(base) <- NULL
  base
}


##' create blank strings with a given number of characters
##' @seealso Examples for \code{\link{regmatches}}
##' @export
blanks <- function(n) {
  vapply(Map(rep.int, rep.int(" ", length(n)), n, USE.NAMES=FALSE),
         paste, "", collapse="")
}

#' list files recursively
#'
#' @param path character vector of path names
#' @param dir_pattern optional regexp
#' @param exclude_pattern optional regexp
#' @param file_pattern optional regexp
#' @export
listFilesRec <- function(path=".",
                         dir_pattern=NULL,
                         exclude_pattern=NULL,
                         file_pattern=NULL) {
  
  stopifnot(require(foreach))
  
  # find directories
  dirs <- listDirs(path, dir_pattern, full.names=TRUE)
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
  files
}

#' list directories only
#'
#' @param path a character vector of path names
#' @param ... Arguments passed on to \code{\link{list.files}}
#' @export
listDirs <- function(path, ...) {
    list.files(path, ...)[file.info(list.files(path, full.names=TRUE))$isdir]
}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
# --R-- vim:fdm=marker:fmr={{{,}}}:fdl=0: