#' Package installer
#' 
#' Wrapper around \code{\link{biocLite}}. In addition to the standard
#' functionality provided \code{biocLite}, this function also download and
#' untars the source files to \code{destdir}.
#' 
#' @param pkgs Packages
#' @param destdir Where to put the source code.
#' @param update call \code{\link{update_packages}}.
#' @param ... Further arguments passed on to \code{\link{biocLite}}.
#' 
#' @export
install_packages <- function (pkgs, destdir = '~/R/Packages',
                              update = FALSE, ...) {
  assert_that(!missing(pkgs), is.character(pkgs))
  BiocInstaller::biocLite(pkgs, suppressUpdates=TRUE, destdir=destdir, ...)
  
  if (update) {
    update_packages(destdir=destdir)
  }
  else {
    destdir <- normalizePath(destdir)
    cwd <- setwd(destdir)
    extract_packages(destdir)
    setwd(cwd)
  }
}


#' Package updater
#' 
#' Check for outdated packages, save the source to \code{destdir},
#' extract the packages in \code{destdir}, and install
#' 
#'  @param destdir Where the source code goes
#'  @importFrom BiocInstaller biocValid
#'  @importFrom BiocInstaller biocLite
#'  @export
update_packages <- function (destdir = '~/R/Packages') {
  destdir <- normalizePath(destdir)
  cwd <- setwd(destdir)
  extract_packages(destdir)
  
  tryCatch(biocValid(), error = function (e) {
    if (grepl("package\\(s\\) out of date", e$message)) {
      message('Updating ', strsplitN(e$message, ' ', 1:2))
      if (interactive())
        ask <- TRUE
      else
        ask <- FALSE
      biocLite(destdir='.', ask=ask)
      extract_packages(destdir)
    }
    else {
      message("Nothing to update")
    }
  })
           
  setwd(cwd)
}

#' @keywords internal
extract_packages <- function (destdir) {
  compressed <- dir(destdir, pattern="gz$")
  if (any(duplicated(strsplitN(compressed, '_', 1)))) {
    stop("Duplicated gzip files in ", destdir,
         ". Resolve manually")
  }
  
  x <- lapply(compressed, untar)
  if (!all(unlist(x) == 0)) {
    stop("Failed to extract ", compressed[unlist(x) != 0])
  }
  
  x <- lapply(compressed, unlink)
  if (!all(unlist(x) == 0)) {
    stop("Failed to unlink ", compressed[unlist(x) != 0])
  }
  
  if (NROW(compressed)) {
    message("Extracted to ", destdir, ": ",
            paste0(compressed, collapse=', '))
  }
}


#' A quick installer for my R packages on GitHub
#' 
#' Prompts for which of the following packages you want to install:
#' \code{rmisc}, \code{Rentrez}, \code{biofiles}, \code{blastr},
#' \code{ncbi}.
#'
#'  @export
update_github <- function () {
  assert_that(require(devtools))
  pkgs <- c("rmisc", "rentrez", "biofiles", "blastr", "ncbi")
  cat("Update packages:\n")
  cat(sprintf("%s) %s", seq_along(pkgs), pkgs), sep="\n")
  
  while (TRUE) {
    msg <- "Which packages do you want to upgrade? [a(ll)|12345|q(uit)]: "
    which <- tolower(strsplit(gsub('\\s+', '', readline(msg)), '')[[1]])
    if (!paste(which, collapse='') %in% c("a","all","q","quit")) {
      if (!any(is.na(idx <- suppressWarnings(as.numeric(which))))
          && max(idx) <= length(pkgs)
          && min(idx) > 0) {
        pkgs <- pkgs[idx]
        break
      }
      else {
        message("Please submit 'all', 'quit', or numbers between 1 and ", length(pkgs))
      }
    }
    else {
      if (which[1] == 'q') 
        pkgs <- ''
      break
    }
  }
  
  if (!all_empty(pkgs))
    #cat(pkgs, sep=", ")
    install_github(pkgs, 'gschofl')
}


#' Require one or more packages
#' 
#' A simple wrapper for \code{\link[base]{require}} that allows
#' loading multiple packages. Issues a warning if a package
#' cannot be loaded.
#' 
#' @param ... Package names (as strings).
#' @seealso
#' \code{\link[base]{require}}
#' @export
require.all <- function(...) {
  pkgs <- as.character(list(...))
  if (all_empty(pkgs)) {
    return(invisible())
  }
  success <- suppressMessages( suppressWarnings(
    unlist(lapply(pkgs, require, character.only=TRUE))
  ))
  if (!all(success)) {
    warning("Failed to load package(s):\n", paste(pkgs[!success], collapse=", "),
            call.=FALSE)
  }
  return(invisible(success))
}

#' Load saved datasets
#' 
#' Reload \code{RData} datasets. Wrapper for \code{\link[base]{load}}
#' that attempts to load all datasets found in a directory.
#' 
#' @param cache Path to directory containing RData-files.
#' @param envir the environment where the data should be loaded.
#' @param verbose print item names during loading.
#' @return A character vector of the names of objects created.
#' @export
load.all <- function (cache = "./cache", envir=.GlobalEnv, verbose=FALSE) {
  rdata <- dir(cache, pattern="rda(ta)?$", full.names=TRUE, ignore.case=TRUE)
  if (all_empty(rdata)) {
    return(invisible())
  }
  rcon <- lapply(normalizePath(rdata), compose(gzcon, gzfile))
  on.exit(lapply(rcon, close))
  success <- lapply(rcon, function (con) {
    tryCatch(load(con, envir=envir, verbose=verbose), error = function (e) e )
  })
  if (any(idx <- vapply(success, is, "error", FUN.VALUE=logical(1)))) {
    warning("Failed to load:\n", paste(rdata[idx], collapse = "\n"), call. = FALSE)
    success <- success[!idx]
  }
  return(unlist(success))
}


#' Open an RStudio project from R
#' 
#' Quickly open a package (ar any  directory in the search path) as an RStudio
#' project. The search paths are defined by the custom options \code{packages},
#'  \code{projects}, and \code{devel}, which I have preset in \code{.Rprofile}.
#'  
#'  @param pkg The name of a package (or project directory) given as a Symbol.
#'  @param path One of 'all', 'packages', 'projects', or 'devel'.
#'  
#'  @return Opens RStudio.
#'  @seealso Inspired by \href{http://stackoverflow.com/questions/18426726/system-open-rstudio-close-connection}{this} question on stackoverflow
#'  @export  
open_Rproj <- function (pkg, path = 'all') {
  
  open_project <- function(rproj) {
    rstudio <- Sys.which("rstudio")
    if (rstudio == "") {
      stop("RStudio is not installed in PATH", call.=TRUE)
    }
    action <- paste(rstudio, rproj)
    system(action, wait=FALSE, ignore.stderr=TRUE)
  }
  
  Rproj.template <- c("Version: 1.0", "", "RestoreWorkspace: Default",
                      "SaveWorkspace: Default",  "AlwaysSaveHistory: Default",
                      "", "EnableCodeIndexing: Yes",  "UseSpacesForTab: Yes",
                      "NumSpacesForTab: 2", "Encoding: UTF-8",  "",
                      "RnwWeave: knitr", "LaTeX: pdfLaTeX")
  
  pkg <- deparse(substitute(pkg))
  path <- match.arg(path, c('all', 'packages', 'projects', 'devel'))
  if (path == 'all') {
    path <- normalizePath(c(getOption("packages"), getOption("projects"), getOption("devel")))
  } else {
    path <- normalizePath(getOption(path))
  }
  
  pkg_path <- grep(pkg, dir(path, full.names=TRUE, ignore.case=TRUE), value=TRUE)
  
  if (length(pkg_path) > 1) {
    warning("Found ", length(pkg_path), " packages of  name ", sQuote(pkg), ".\n",
            "Will open the first: ", sQuote(pkg_path[1]), call.=FALSE, immediate.=TRUE)
    pkg_path <- pkg_path[1]
  } else if (length(pkg_path) < 1) {
    stop("Package ", sQuote(pkg), " not found.", call.=FALSE)
  }
  
  rproj_loc <- dir(pkg_path, pattern="*.Rproj", full.names=TRUE)
  if (length(rproj_loc) < 1) {
    rproj_loc <- file.path(pkg_path, paste0(pkg, '.Rproj'))
    cat(paste(Rproj.template, collapse = "\n"), file = rproj_loc)  
  }
  
  open_project(rproj_loc)
}

