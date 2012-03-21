##' Retrieve genomes from a remote database
##'
##' works currently with bacterial genomes from NCBI
##'
##' @param db which remote database. Currently only NCBI
##' @param dest download directory
##' @param which_files regular expression matching the extensions of the files
##' to be downloded. E.g. "gff$|fna$"
##' @param which_dirs regular expression matching the genome directories to
##' download from
##' @param keep_dirs regular expression matching local directories that shouldn't
##' be touched
##' @param update_dirs if \code{TRUE} new directories matching \code{which_dirs}
##' will be downloaded
##' @param update_files if \code{TRUE} files in locally existing dirctories will
##' be updated if newer versions exist in the remote database
##' @param start_from_scratch
##' @param SORT
##' @param VERBOSE
##'
##' @export
fetchGenomes <- function(db="NCBI",
                         dest=".", 
                         which_files="gbk$",
                         which_dirs="",
                         keep_dirs=NULL,
                         update_dirs=TRUE,
                         update_files=TRUE,
                         start_from_scratch=FALSE,
                         SORT=FALSE,
                         VERBOSE=FALSE) {
  
  stopifnot(require(RCurl))
  stopifnot(require(stringr))
  
  # get contents of remote directory 
  URL <- switch(db,
                NCBI = "ftp://ftp.ncbi.nih.gov/genbank/genomes/Bacteria/"
                )
  src_dirs <- strsplit(getURL(URL, dirlistonly = TRUE), "\n", fixed = TRUE)[[1]]
  src_dirs <- src_dirs[grepl(which_dirs, src_dirs)]

  # get contents of destination directory
  if (!file.exists(dest)) dir.create(dest)
  dest_dirs <- listDirs(dest, full.names=TRUE)
  dest_dirs <- dest_dirs[grepl(which_dirs, dest_dirs)]

  if (isTRUE(update_dirs)) {
  # remove obsolete local directories and fetch new ones
  # keep those directories specified in "keep_dirs"
    rm_dirs <- dest_dirs[!basename(dest_dirs) %in% src_dirs]
    if (length(rm_dirs) != 0) {
      if (!is.null(keep_dirs) && any(grepl(keep_dirs, rm_dirs))) {
        rm_dirs <- rm_dirs[-grep(keep_dirs, rm_dirs)]
      }
      unlink(rm_dirs, recursive=TRUE)
    }

    # get the names of new src directories
    new_dirs <- src_dirs[!src_dirs %in% basename(dest_dirs)]

    opts <- curlOptions(ftp.use.epsv=FALSE, forbid.reuse=TRUE)
    curl <- getCurlHandle(.opts=opts)
    
    for (d in new_dirs) {
      # create the local directory
      if (!file.exists(file.path(dest, d))) dir.create(file.path(dest, d))

      # get the names of the files in the source directory
      file_names <- getURL(str_c(URL, d, "/"), dirlistonly=TRUE)
      file_names <- grep(which_files, str_c(URL, d, "/", strsplit(file_names, "\n")[[1]]), value=TRUE)
      for (dn in file_names) {
        target <- file.path(dest, d, basename(dn))
        contents <- getURL(dn, curl=curl, verbose=VERBOSE)
        if (nchar(contents) > 0) cat(contents, file=target)
      }
    }
  }

  if (isTRUE(update_files)) {
    upd_dirs <- src_dirs[which(src_dirs %in% basename(dest_dirs))]
    if (isTRUE(SORT)) {
      upd_dirs <- sort(upd_dirs)
    } else {
      upd_dirs <- sample(upd_dirs, length(upd_dirs))
    }

    # check for completed downloads to avoid repetition
    if (file.exists(file.path(dest, "completed.downloads"))) {
      if (isTRUE(start_from_scratch)) {
        unlink(file.path(dest, "completed.downloads"))
      } else {
        completed <- scan(file.path(dest, "completed.downloads"), "char")
        completed <- grep(paste(completed, collapse="|"), upd_dirs)
        if(length(completed != 0)) upd_dirs <- upd_dirs[-completed]
      }
    }

    opts <- curlOptions(timecondition=TRUE, ftp.use.epsv=FALSE,
                        forbid.reuse=TRUE, filetime=TRUE)
    curl <- getCurlHandle(.opts=opts)
    for (d in upd_dirs) {
      file_names <- getURL(str_c(URL, d, "/"), ftplistonly = TRUE)
      file_names <- grep(which_files, str_c(URL, d, "/", strsplit(file_names, "\n")[[1]]), value=TRUE)
      keep <- list.files(grep(d, dest_dirs, value=TRUE)) %in% basename(file_names)
      # delete those types of files that are not in the "which_files" list
      if (any(!isTRUE(keep))) {
        unlink(list.files(grep(d, dest_dirs, value=TRUE), full.names=TRUE)[!keep])
      }

      for(dn in file_names) {
        target <- file.path(dest, d, basename(dn))
        TIME <- unclass(file.info(target)$ctime)
        time_value <- curlOptions(timevalue=TIME)
        contents <- getURL(dn, curl=curl, .opts=time_value, verbose=VERBOSE)
        if (nchar(contents) > 0) cat(contents, file=target)
      }
      cat(str_c(d, "\n"), file=file.path(dest, "completed.downloads"), append=TRUE)
    }
  }
}



# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
# --R-- vim:fdm=marker:fmr={{{,}}}:fdl=0:

