#' Customised \code{\link[utils]{install.packages}}
#'
#' Puts source code in a specified \emph{packages} directory and
#' calls \code{\link{generate_tag_files}}
#'
#' @param pkgs character vector of package names
#' @param ... Arguments to be passed on to \code{\link[utils]{install.packages}}
#' @param check_updates if \code{TRUE} run \code{\link[utils]{old.packages}}
#' @param tags if \code{TRUE} call \code{\link{generate_tag_files}}
#' @param repos one of "CRAN", "bioc", "Omegahat", "R-Forge", "github", or
#' "bitbucket". Defaults to "CRAN"
#'
#' @import foreach
#' @import iterators
#' @export
install_packages <- function(pkgs="",
                             repos=c("bioc", "CRAN", "Omegahat", "R-Forge",
                                     "github", "bitbucket"),
                             username="gschofl",
                             branch="master",
                             ...,
                             tags=TRUE) {
  
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
      # if the package is present in the packages directory delete
      # it and do a fresh install
      rm_files <- dir(pkg_dir, full.names=TRUE)
      rm_files <- rm_files[grepl(paste(pkgs, collapse="|"), rm_files)]
      unlink(rm_files, recursive=TRUE)
    }
    
    # install from CRAN or Bioconductor
    if (repos == "bioc") {
      biocLite(pkgs=pkgs, destdir=pkg_dir, ...)
    } else if (repos %in% c("github","bitbucket")) {
      installFromHost(repo=pkgs, username=username, branch=branch, host=repos, ...)
    } else {
      install.packages(pkgs, destdir=pkg_dir, ...)
    }
    # expand tarballs in destdir and generate tags
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
      c(path.expand(file.path(pkg_dir, pkgs)), strip_ext(expand_files, "_"))
    } else {
      path.expand(strip_ext(expand_files, "_"))
    }

    generate_tag_files(target_dirs, verbose=TRUE)
    
  } else {
    if (repos == "bioc") {
      biocLite(pkgs=pkgs, destdir=pkg_dir, ...)
    } else if (repos %in% c("github","bitbucket")) {
      installFromHost(repo=pkgs, username=username, branch=branch, host=repos, ...)
    } else {
      install.packages(pkgs, destdir=pkg_dir, ...)
    }
  }
}


#' @keywords internal
generate_tag_files <- function(target_dirs=NULL,
                               rtags="RTAGS",
                               ctags="RsrcTags",
                               tag_file_path=getOption("packages"),
                               verbose=FALSE) {
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
    for (i in seq_along(target_dirs)) {
      
      rtags(target_dirs[i], recursive=TRUE, ofile=tag_files[1], 
            append=shall_append[i], verbose=verbose)
      
      a <- if (shall_append[i]) " -a " else " "
      system(paste0("ctags -R", a, "-f ", tag_files[2],
                    " --languages=C,C++,Fortran ", target_dirs[i]))
    }
  } else {
    for (tag_file in tag_files) {
      removeTags(target_dirs, tag_file)
    }
    for (target in target_dirs) {
      rtags(target, recursive=TRUE, ofile=tag_files[1], append=TRUE,
            verbose=verbose)
      system(paste0("ctags -R -a -f ", tag_files[2],
                    " --languages=C,C++,Fortran ", target))
    } 
  }
  
  unlink(file.path(getOption("vim"), rtags))
  file.symlink(tag_files[1], file.path(getOption("vim"), rtags))
  unlink(file.path(getOption("vim"), ctags))
  file.symlink(tag_files[1], file.path(getOption("vim"), ctags))
  invisible(TRUE)
}


#' @keywords internal
installFromHost <- function (repo = "rmisc",
                             username = "gschofl",
                             branch = "master",
                             host = c("github","bitbucket"),
                             ...) {
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


#' @author Hadley Wickham <h.wickham@@gmail.com>
#' @keywords internal
installUrl <- function (url, name = NULL, ...){
  if (is.null(name)) {
    name <- rep(list(NULL), length(url))
  }
  invisible(mapply(installUrlSingle, url, name, ...))
}


#' @author Hadley Wickham <h.wickham@@gmail.com>
#' @importFrom RCurl curlOptions
#' @importFrom RCurl getCurlHandle
#' @importFrom RCurl getURL
#' @importFrom RCurl getBinaryURL
#' @importFrom devtools install
#' @keywords internal
installUrlSingle <- function (url, name = NULL, ...) {
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
    strip_ext(bundle)
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

