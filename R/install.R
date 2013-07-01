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
      biocLite(destdir='.')
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