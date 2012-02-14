##' Perform a global regular expression match
##' 
##' Returns the match, a capture group, or an empty string if the
##' match fails
##' 
##' @param pattern a character string containing a regular expression.
##' @param str a character vector where matches are sought.
##' @param i (optional) number or name of capture group.
##' @param perl if \code{TRUE} perl-compatible regexps are used
##' @return a list containing the matches the matches of the specifies
##' capture group(s)
##' @export
##' @examples
##' ##
Match  <-  function (pattern, str, i=NULL, perl=TRUE) {
  if (is.null(i)) {
    strmatch(pattern, str, perl=perl, capture=FALSE)
  }
  else {
    lapply(strmatch(pattern, str, perl=perl, capture=TRUE)[["capture"]], "[", i)
  }
}

#' Extract matched group(s) from a string.
#'
#' @param pattern character string containing a regular expression
#' @param str character vector where matches are sought
#' @param capture if \code{TRUE} capture groups are returned in addition
#' to the complete match
#' @param perl if \code{TRUE} perl-compatible regexps are used.
#' @param global if \code{TRUE} \code{\link{gregexp}} is used for matching
#' otherwise \code{regexpr}.
#' @param ignore.case case sensitive matching
#' @return a list containing a \code{match} and a \code{capture} component
#' @keywords character
#' @export
#' @examples
#' ##
strmatch <- function (pattern, str, capture=TRUE, perl=TRUE, global=TRUE, ignore.case=FALSE) {

  if (!is.atomic(str))
    stop("String must be an atomic vector", call. = FALSE)

  if (!is.character(str)) 
    string <- as.character(str)

  if (!is.character(pattern)) 
    stop("Pattern must be a character vector", call. = FALSE)

  if (global)
    m <- gregexpr(pattern, str, perl=perl, ignore.case=ignore.case)
  else
    m <- regexpr(pattern, str, perl=perl, ignore.case=ignore.case)

  .matcher <- function (str, m) {
    Map( function (str, start, len) substring(str, start, start + len - 1L), 
         str, m, lapply(m, attr, "match.length"), USE.NAMES=FALSE)
  }

  match <- if (capture) {
    .capture.matcher <- function (str, m) {
      cap <- Map( function (str, start, len) {
        Map( function (str, start, len) {
          substr(str, start, start + len - 1L) 
        }, str, start, len, USE.NAMES=FALSE)
      }, str, lapply(m, attr, "capture.start"),
                  lapply(m, attr, "capture.length"), USE.NAMES=FALSE)

      cap <- Map( function (val, name) `names<-`(val, name),
                  cap, lapply(m, attr, "capture.names"), USE.NAMES=FALSE)
      cap
    }

    list(match=.matcher(str, m),
         capture=if (!is.null(attributes(m[[1]])$capture.start))
           .capture.matcher(str, m) else NULL)
  } else {
    match <- .matcher(str, m)
  }
  match
}



# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

