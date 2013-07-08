#' Strip file extensions
#'
#' Strips the extension (or an arbitrary tag) from a file name. 
#' 
#' @param file The file name(s).
#' @param sep specifies the seperator character (default ".").
#' @param level How many extensions should be stripped.
#' The default (0) strips all, 1 strips the last one, 2 strips the last two,
#' and so on.
#' 
#' @export
strip_ext <- stripExt <- function (file, sep="\\.", level=0) {
  assert_that(!missing(file), is.character(file))
  if (level == 0L) {
    # level 0 ditches everything that comes after a dot
    vapply(file, function(x) usp(x, sep)[1L], character(1),
           USE.NAMES = FALSE)
  } else if (level > 0L) {
    # level 1 removes the very last extension: file.xyz.abc > file.xyz
    # level 2: file.xyz.abc > file
    # and so on
    count <- count_re(file, sep) + 1L - level
    # to always grab at least the first element after the split
    # reset zero counts to 1
    count <- ifelse(count < 1, 1, count)
    unlist(Map(function(x, lvl) {
      paste0(usp(x, sep)[ seq_len(lvl) ], collapse = gsub('\\', '', sep, fixed=TRUE))
    }, x=file, lvl=count, USE.NAMES=FALSE))
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
    replacement <- usp(replacement, split="^\\.")[2L]
  }
  paste(strip_ext(file=file, sep=sep, level=level), replacement,
        sep=gsub("\\", "", sep, fixed=TRUE))  
}

