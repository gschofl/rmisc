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
                                    "tests"))
{
  
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
##' 
##' \itemize{
##' \item{\code{vim}}{path to your \file{~/.vim/} directory.}
##' \item{\code{packages}}{path to a directory containing all the R source
##' code, e.g. \file{~/R/packages}.}
##' \item{\code{devel}}{path to your own development directory, e.g. 
##' \file{~/R/Devel}.}
##' }
##'
##' \code{generate_tag_files} will look recursively for R code in the
##' specified directories and in the case of \code{packages} also for C,
##' Fortran, Java and Tcl code.
##' 
##' @usage generate_tag_files()
##' 
##' @export
generateTagFiles <- function(target_dirs=NULL,
                             rtags="RTAGS",
                             ctags="RsrcTags",
                             tag_file_path=getOption("packages"),
                             verbose=FALSE)
{
  message("Building Tags ...")
  
  removeTags <- function (target_dirs, tag_file) {
    
    if (file.exists(tag_file)) {
      conr <- file(tag_file, open="r")
    } else {
      return(invisible(NULL))
    }
    
    tag_lines <- readLines(conr)
    close(conr)
    target_pattern <- paste(target_dirs, collapse="|")
    start_idx <- grep(target_pattern, tag_lines)
    if (length(start_idx) > 0) {
      f.idx <- grep("\\f", tag_lines)
      if (length(f.idx) == 0L) {
        end_idx <- start_idx
      } else {
        end_idx <- vapply(start_idx, function (i)
          f.idx[which(f.idx > i)[1]], integer(1))
      }
      end_idx[is.na(end_idx)] <- start_idx[is.na(end_idx)]
      idx <- Map(seq, start_idx, end_idx)
      tag_lines <- tag_lines[-unlist(idx)]
      conw <- file(tag_file, open="w")
      writeLines(tag_lines, conw)
      close(conw)
    }
    invisible(TRUE)
  }
  
  tag_files <- file.path(tag_file_path, c(rtags, ctags))
  
  if (is.null(target_dirs)) {
    unlink(tag_files)
    target_dirs <- c(getOption("devel"),getOption("packages"),getOption("base"))
    shall_append <- c(FALSE, TRUE, TRUE)
    styles <- list(c("U","U","G"), c("U","U","U","G"), c("U","U","U","G"))
    for (i in seq_along(target_dirs)) {
      
      rtags(target_dirs[i], recursive=TRUE, ofile=tag_files[1], 
            append=shall_append[i], verbose=verbose)
      
      SysCall("ctags", opts=list(R=TRUE, a=shall_append[i], f=tag_files[2]),
              params=list(languages="C,C++,Fortran"), style=styles[[i]],
              args=target_dirs[i], show_cmd=FALSE)
    }
  } else {
    for (tag_file in tag_files) {
      removeTags(target_dirs, tag_file)
    }
    for (target in target_dirs) {
      rtags(target, recursive=TRUE, ofile=tag_files[1], append=TRUE,
            verbose=verbose)
      SysCall("ctags", opts=list(R=TRUE, a=TRUE, f=tag_files[2]),
              params=list(languages="C,C++,Fortran"),
              style=c("U","U","U","G"), args=target, show_cmd=FALSE)
    } 
  }
  
  unlink(file.path(getOption("vim"), rtags))
  file.symlink(tag_files[1], file.path(getOption("vim"), rtags))
  unlink(file.path(getOption("vim"), ctags))
  file.symlink(tag_files[1], file.path(getOption("vim"), ctags))
  invisible(TRUE)
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
##' @param repos one of "CRAN", "bioc", "Omegahat", "R-Forge", "github", or
##' "bitbucket". Defaults to "CRAN"
##'
##' 
##' @export
installPackages <- function(pkgs="",
                            repos=c("bioc", "CRAN", "Omegahat", "R-Forge", "github", "bitbucket"),
                            username="gschofl",
                            branch="master",
                            ...,
                            tags=TRUE)                         
{
  
  repos <- match.arg(repos)
  
  if (!repos %in% c("github","bitbucket")) {
    ops <- options("repos")
    setRepositories(ind=1:20)
    all_repos <- getOption("repos")
    options(ops)
    select_repos <- c(bioc="bioc", all_repos[c("CRAN", "Omegahat", "R-Forge")])
    
    if (is.na(select_repos[repos]))
      stop("Selected Repository not available")
    
    switch(repos,
           bioc = options(repos=biocinstallRepos()),
           options(repos=select_repos[repos]))
  }
  
  pkg_dir <- path.expand(getOption("packages"))
  if (isTRUE(tags)) {

    if (all(nzchar(pkgs))) {
      ## if the package is present in the packages directory delete
      ## it and do a fresh install
      rm_files <- dir(pkg_dir, full.names=TRUE)
      rm_files <- rm_files[grepl(paste(pkgs, collapse="|"), rm_files)]
      unlink(rm_files, recursive=TRUE)
    }
    
    ## install from CRAN or Bioconductor
    if (repos == "bioc") {
      biocLite(pkgs=pkgs, destdir=pkg_dir, ...)
    } else if (repos %in% c("github","bitbucket")) {
      installFromHost(repo=pkgs, username=username, branch=branch, host=repos, ...)
    } else {
      install.packages(pkgs, destdir=pkg_dir, ...)
    }
    ## expand tarballs in destdir and generate tags
    all_files <- dir(pkg_dir, full.names=TRUE)
    tar_files <- all_files[grepl(".*tar\\.gz$", all_files)]
    dirs <- all_files[file.info(all_files)$isdir]
    if (length(dirs) == 0L) dirs <- "XXX"
    expand_files <- tar_files[!grepl(paste0(gsub("^\\./", "", dirs), "_", collapse="|"), tar_files)]

    if (length(expand_files)) {
      foreach(f=iter(expand_files)) %do% 
        untar(f, exdir=pkg_dir, verbose=TRUE)
    }
    
    target_dirs <- if (repos == "github") {
      c(path.expand(file.path(pkg_dir, pkgs)), stripExt(expand_files, "_"))
    } else {
      path.expand(stripExt(expand_files, "_"))
    }

    generateTagFiles(target_dirs, verbose=TRUE)
    
  } else {
    if (repos == "bioc") {
      biocLite(pkgs=pkgs, ...)
    } else if (repos == "github") {
      installGithub(repo=pkgs, username=username, branch=branch, ...)
    } else {
      install.packages(pkgs, ...)
    }
  }
  options(ops)
}
                            
##' Strip file extensions
##'
##' @param file file name(s)
##' @param sep specifies the seperator character (default ".").
##' @param level numeric denoting how many extensions should be stripped.
##' The default (0) strips all, 1 strips the last one, 2 strips the last two,
##' and so on.
##' 
##' @export
stripExt <- stripExtension <- function (file, sep="\\.", level=0)
{
  if (level == 0L) {
    # level 0 ditches everything that comes after a dot
    base <- vapply(file, function(x) {
      strsplit(x, sep)[[1L]][1L]
    }, FUN.VALUE = character(1))
    names(base) <- NULL
    return(base)
  }
  else if (level > 0L) {
    # level 1 removes the very last extension: file.xyz.abc > file.xyz
    # level 2: file.xyz.abc > file
    # and so on
    count <- vapply(file, function (x) {
      length(gregexpr(pattern=sep, text=x)[[1L]])
    }, FUN.VALUE = integer(1)) + 1 - level
    count <- ifelse(count < 1,  1, count)
    
    base <- mapply( function(x, level) {
      paste(strsplit(x, sep)[[1L]][seq(1, level)],
            collapse=gsub("\\", "", sep, fixed=TRUE))
    }, file, count, SIMPLIFY=TRUE, USE.NAMES=FALSE)
    
    return(base)
  }
  else {
    stop(sprintf("Level %s is invalid. Must be 0, 1, 2, ...", sQuote(level)))
  }
}

# testing
# stripExt("test.ext")
# stripExt("my.test.ext", level=1)
# stripExt("my.test.ext", level=2)
# stripExt("my.test.ext", level=3)
# stripExt("myTest_ext", sep="_")
# stripExt(c("mytest1.ext","mytest2.ext"))
# replaceExt("meinFile.xls", "xlsx")


##' Replace file extensions
##' 
##' @inheritParams stripExt
##' @param replacement replacement extension
##'
##' @export
replaceExt <- replaceExtension <- function (file,
                                            replacement="",
                                            sep="\\.",
                                            level=0)
{
  if (nchar(replacement) == 0L)
    sep=""
  # strip a leading "." from replacement
  if (grepl("^\\.", replacement)) {
    replacement <- strsplit(replacement, split="^\\.")[[1L]][2L]
  }
  
  return(paste(stripExt(file=file, sep=sep, level=level),
               replacement, sep=gsub("\\", "", sep, fixed=TRUE)))
}

# testing
# replaceExt("test.ext", "new_ext")
# replaceExt(c("test1.ext","test2.ext"), "new_ext")
# replaceExt("my.test.ext", "new_ext", level=1)
# replaceExt("my.test.ext", "new_ext", level=2)
# replaceExt("my.test.ext", "new_ext", level=3)
# replaceExt("myTest_ext", "new_ext", sep="_")

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
                         file_pattern=NULL)
{  
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


##' @keywords internal
installFromHost <- function (repo = "rmisc",
                             username = "gschofl",
                             branch = "master",
                             host = c("github","bitbucket"),
                             ...) 
{
  host <- match.arg(host)
  
  message(paste("Installing", host,  "repo "),
          paste(repo, branch, sep = "/", collapse = ", "), " from ", 
          paste(username, collapse = ", "))
  
  url <- switch(host,
                github=paste0("https://github.com/", username, "/", repo, 
                              "/zipball/", branch),
                bitbucket=paste0("https://bitbucket.org/", username, "/", repo, 
                                 "/get/", branch, ".zip")
  )
  
  name <- switch(host,
                 github=paste0(repo, ".zip"),
                 bitbucket=paste0(branch, ".zip")
                 )
  
  installUrl(url, name, ...)
}


##' @author Hadley Wickham <h.wickham@@gmail.com>
##' @keywords internal
installUrl <- function (url, name = NULL, ...) 
{
  if (is.null(name)) {
    name <- rep(list(NULL), length(url))
  }
  invisible(mapply(installUrlSingle, url, name, ...))
}

##' @author Hadley Wickham <h.wickham@@gmail.com>
##' @keywords internal
installUrlSingle <- function (url, name = NULL, ...) 
{
  if (is.null(name)) {
    name <- basename(url)
  }
  message("Installing ", name, " from ", dirname(url))
  bundle <- file.path(getOption("packages"), name)
  content <- RCurl::getBinaryURL(url, .opts = list(followlocation = TRUE, 
                                            ssl.verifypeer = FALSE))
  writeBin(content, bundle)
  outdir <- if (grepl("bitbucket", url)) {
    dirname(as.character(unzip(bundle, list = TRUE)$Name[1]))
  } else {
    basename(as.character(unzip(bundle, list = TRUE)$Name[1]))
  }
  exdir <- if (grepl("bitbucket", url)) {
    file.path(dirname(bundle), str_split_fixed(outdir, "-", 3)[,2])
  } else {
    stripExt(bundle)
  }
  unzip(bundle, overwrite=TRUE, exdir=exdir)
  pkg_path <- file.path(exdir, outdir)
  if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
    stop("Does not appear to be an R package", call. = FALSE)
  }
  config_path <- file.path(pkg_path, "configure")
  if (file.exists(config_path)) {
    Sys.chmod(config_path, "777")
  }
  devtools::install(pkg_path, ...)
}

##' Purge rows containing NAs from a data frame
##' 
##' returns the supplied colums without rows containing NAs as a data frames
##' 
##' @param df data frame
##' @param cols column names
##' 
##' @return data frame
##' 
##' @export
purgeNA <- function (df, cols) {
  if (length(cols) <= 1) {
    stop("Brauche mindestens zwei Spaltennamen um einen Datenrahmen zu bauen")
  }
  if(!all(cols %in% names(df))) {
    stop("Einen oder mehrere der Spaltennamen gibt es nicht im Datenrahmen")
  }
  df <- df[, names(df) %in%cols]
  df <- df[!Reduce("|", lapply(df, is.na)), ]
  return(df)
}

##' benchmark a function
##' 
##' @param f function call
##' @param rep replicate runs
##' 
##' @export
benchmark <- function(f, rep) {
  mean(replicate(rep, system.time(eval(substitute(f))))["elapsed",])
}


##' chain functions
##' 
##' @param x object
##' @param f function
##' 
##' @export
`%@%` <- function(x, f) {
  eval.parent(as.call(append(as.list(substitute(f)), list(x), 1)))
}


# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
# --R-- vim:fdm=marker:fmr={{{,}}}:fdl=0:
