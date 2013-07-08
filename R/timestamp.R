#' @importFrom RCurl basicTextGatherer curlPerform
#' @importFrom assertthat is.readable
NULL

#' Compare local and remote timestamp
#' 
#' @param url A string giving the URL.
#' @param file The local file.
#' @param .message A message
#' 
#' @return \code{FALSE} if the local file is more recent than the remote
#' source, else \code{TRUE}
#' @export
check_timestamp <- function (url, file, .message) {
  assert_that(is.readable(file))
  local_time <- file.info(file)$ctime
  
  h <- basicTextGatherer()
  failed = FALSE
  status <- tryCatch(curlPerform(url = url, followlocation = TRUE, filetime=TRUE,
                                 headerfunction = h$update, nobody = TRUE),
                     COULDNT_RESOLVE_HOST = function(x) failed <<- TRUE, 
                     error = function(x) failed <<- TRUE)
  
  if (failed)
    stop("Failed to connect to ", sQuote(url))
  
  remote_time <- grep("Last-Modified: ", usp(h$value(), "\r\n"), value=TRUE)
  remote_time <- as.POSIXct(strptime(sub("Last-Modified: ", "", remote_time),
                                     format="%a, %d %b %Y %H:%M:%S GMT", tz="GMT"))
  
  if (remote_time < local_time) {
    if (.message)
      message("Local file is more recent than url.")
    FALSE
  }
  else
    TRUE 
}
