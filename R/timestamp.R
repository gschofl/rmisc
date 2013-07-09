#' @importFrom RCurl basicTextGatherer curlPerform
#' @importFrom assertthat is.readable
NULL

#' Compare local and remote timestamp
#' 
#' @param url A string giving the URL.
#' @param file The local file.
#' @param .message A message
#' 
#' @return \code{TRUE} if the remote source is more recent than the local
#' file, or if the local file does not exist, else \code{FALSE}
#' @export
check_timestamp <- function (url, file, .message) {
  assert_that(is.string(url), is.string(file))
  if (!file.exists(file)) {
    return(TRUE)
  }
  local_time <- file.info(file)$ctime
  
  h <- basicTextGatherer()
  failed = FALSE
  status <- tryCatch(curlPerform(url = url, followlocation = TRUE, filetime=TRUE,
                                 headerfunction = h$update, nobody = TRUE),
                     COULDNT_RESOLVE_HOST = function(x) failed <<- TRUE, 
                     error = function(x) failed <<- TRUE)
  
  if (failed)
    stop("Failed to connect to remote source: ", sQuote(url))
  
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  remote_time <- grep("Last-Modified: ", usp(h$value(), "\r\n"), value=TRUE)
  remote_time <- as.POSIXct(strptime(sub("Last-Modified: ", "", remote_time),
                                     format="%a, %d %b %Y %H:%M:%S", tz="GMT"))
  Sys.setlocale("LC_TIME", lct)
  
  if (remote_time < local_time) {
    if (.message)
      message("Local file is more recent than the remote source.")
    FALSE
  }
  else
    TRUE 
}
