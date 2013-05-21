#' Strip file extensions
#'
#' @param file file name(s)
#' @param sep specifies the seperator character (default ".").
#' @param level numeric denoting how many extensions should be stripped.
#' The default (0) strips all, 1 strips the last one, 2 strips the last two,
#' and so on.
#' 
#' @export
strip_ext <- stripExt <- function (file, sep="\\.", level=0) {
  if (level == 0L) {
    # level 0 ditches everything that comes after a dot
    base <- vapply(file, function(x) {
      strsplit(x, sep)[[1L]][1L]
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    return( base )
    
  } else if (level > 0L) {
    # level 1 removes the very last extension: file.xyz.abc > file.xyz
    # level 2: file.xyz.abc > file
    # and so on
    count <- vapply(file, function (x) {
      length(gregexpr(pattern=sep, text=x)[[1L]])
    }, FUN.VALUE = integer(1)) + 1 - level
    count <- ifelse(count < 1,  1, count)
    
    base <- mapply( function(x, level) {
      paste(strsplit(x, sep)[[1L]][seq(1, level)],
            collapse=gsub("\\", "", sep, fixed=TRUE))
    }, file, count, SIMPLIFY=TRUE, USE.NAMES=FALSE)
    return( base )
    
  } else {
    stop(sprintf("Level %s is invalid. Must be 0, 1, 2, ...", sQuote(level)))
  }
}


#' Replace file extensions
#' 
#' @inheritParams strip_ext
#' @param replacement replacement extension
#'
#' @export
replace_ext <- replaceExt <- function (file, replacement="", sep="\\.", level=0) {
  if (nchar(replacement) == 0L)
    sep=""
  # strip a leading "." from replacement
  if (grepl("^\\.", replacement)) {
    replacement <- strsplit(replacement, split="^\\.")[[1L]][2L]
  }
  
  return(paste(strip_ext(file=file, sep=sep, level=level),
               replacement, sep=gsub("\\", "", sep, fixed=TRUE)))
}

