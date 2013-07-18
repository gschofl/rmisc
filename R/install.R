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

