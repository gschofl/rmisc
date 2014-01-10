#' @include functional.R
NULL

#' Open an RStudio project from R
#' 
#' Quickly open a package (ar any  directory in the search path) as an RStudio
#' project. The search paths are defined by the custom options \code{rmisc.pkgs},
#' \code{rmisc.proj}, and \code{rmisc.devel}, which I have preset in \code{.Rprofile}.
#' If none of these is defined \code{rproj} will fall back to the current working
#' directory.
#'  
#' @param pkg The name of a package (or project directory) given as a Symbol.
#' @param path One of 'all', 'pkgs', 'proj', or 'devel'.
#'  
#' @return Opens RStudio.
#' @seealso Inspired by \href{http://stackoverflow.com/questions/18426726/system-open-rstudio-close-connection}{this} question on stackoverflow
#' @export  
rproj <- function(pkg, path = 'all') {
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

  if (is.name(pkg)) {
    pkg <- trim(deparse(substitute(pkg)), trim="\"")  
  } else if (is.string(pkg)) {
    pkg <- pkg
  } else {
    stop("'pkg' must be a symbol or a string")
  }
  
  devel.path <- getOption('rmisc.devel') %||% '.'
  proj.path  <- getOption('rmisc.proj') %||% '.'
  pkgs.path  <- getOption('rmisc.pkgs') %||% '.'
  path <- switch(path,
                 all = normalizePath(unique(c(devel.path, proj.path, pkgs.path))),
                 devel = normalizePath(devel.path),
                 proj = normalizePath(proj.path),
                 pkgs = normalizePath(pkgs.path),
                 normalizePath(path, mustWork=TRUE))
  
  pkg_path <- grep(pkg, dir(path, full.names=TRUE, ignore.case=TRUE), value=TRUE)
  
  while (nunique(basename(pkg_path)) > 1L) {
    pkg_path <- unique(dirname(pkg_path))
  }
  
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

#' Create a new \href{http://projecttemplate.net/getting_started.html}{ProjectTemplate}
#' project and open RStudio in it.
#' 
#' @param proj The name of the project directory.
#' @param path Base directory of the project.
#' @param minimal Create a minimal project or a full project.
#' @return No value is returned; RStudio is opened in the newly created project.
#' @export
new_proj <- function(proj, path = getOption("rmisc.proj"), minimal = FALSE) {
  stopifnot(require(ProjectTemplate), !missing(proj))
  proj.path <- normalizePath(file.path(path, proj), mustWork=FALSE)
  assert_that(is.writeable(dirname(proj.path)))
  if (!file.exists(proj.path)) {
    create.project(project.name=proj.path, minimal=minimal)
  }
  rproj(eval(proj), path=path)
}


