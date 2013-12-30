#' @importFrom RCurl basicTextGatherer curlPerform
#' @importFrom assertthat is.readable
NULL


#' Compare local and remote file timestamp and/or size
#' 
#' @param file Path to local file.
#' @param url URL of remote source.
#' @param time Compare the timestamps (default: \code{TRUE}).
#' @param size Compare the file sizes (default: \code{FALSE}).
#' @param .message Print a message
#' 
#' @return \code{TRUE} if the local file does not exist,
#' if the remote source is more recent than the local file,
#' or if the size of the remote source differs from the local file.
#' Otherwise return \code{FALSE}.
#' @export
file_compare <- function(file, url, time = TRUE, size = FALSE, .message = TRUE) {
  assert_that(is.string(url), is.string(file))
  if (!file.exists(file)) {
    return(TRUE)
  }
  file_info <- file.info(file)
  local_time <- file_info$ctime
  local_size <- file_info$size
  
  h <- basicTextGatherer()
  failed <- FALSE
  status <- tryCatch(curlPerform(url = url, followlocation = TRUE, filetime=TRUE,
                                 headerfunction = h$update, nobody = TRUE),
                     COULDNT_RESOLVE_HOST = function(x) failed <<- TRUE, 
                     error = function(x) failed <<- TRUE)
  if (failed) {
    stop("Failed to connect to remote source: ", sQuote(url))
  }
  
  with_localtime("C", {
    header <- usplit(h$value(), "\r\n")
    remote_time <- grep("Last-Modified: ", header, value=TRUE)
    remote_time <- as.POSIXct(strptime(sub("Last-Modified: ", "", remote_time),
                                       format="%a, %d %b %Y %H:%M:%S", tz="GMT"))
    remote_size <- grep("Content-Length: ", header, value=TRUE)
    remote_size <- as.numeric(sub("Content-Length: ", "", remote_size))
  })
  
  if (time && remote_time < local_time) {
    if (.message) {
      message("Local file is more recent than the remote source.")
    }
    FALSE
  }
  else if (size && remote_size != local_size) {
    if (.message) {
      message("Local file differs in size from the remote source.")
    }
    FALSE
  }
  else {
    TRUE
  }
}
