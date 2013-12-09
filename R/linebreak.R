#' Format paragraphs
#' 
#' Similar to \code{\link{strwrap}} but returns a single string with
#' linefeeds inserted
#' 
#' @param s a character vector or a list of character vectors
#' @param width a positive integer giving the column for inserting
#' linefeeds
#' @param indent an integer giving the indentation of the first line of
#' the paragraph; negative values of \code{indent} are allowed and reduce
#' the width for the first line by that value.
#' @param offset a non-negative integer giving the indentation of all
#' but the first line
#' @param split regular expression used for splitting. Defaults to
#' a whitespace character.
#' @param FORCE Words are force split if the available width is too small.
#' @param FULL_FORCE Lines are split exactly at the specified width
#' irrespective of whether there is whitespace or not.
#' 
#' @return a character vector
#' @export
linebreak <- function(s, width = getOption("width") - 2,
                      indent = 0, offset = 0, split = " ",
                      FORCE = FALSE, FULL_FORCE = FALSE) {
  if (!is.character(s)) {
    s <- as.character(s)
  }
  if (all_empty(s)) {
    return("")
  }
  .first_iteration <- TRUE
  # set indent string to "" if a negative value is given
  # this lets us shrink the available width for the first line by that value
  indent_string <- blanks(indent)
  offset_string <- paste0("\n", blanks(offset))
  
  ans <- Map(function(s, width, offset, indent,
                      indent_string, split, FORCE,
                      FULL_FORCE) {
    # remove leading and trailing blanks
    # convert newlines, tabs, spaces to " "
    # find first position where 'split' applies
    if (!FULL_FORCE) {
      s <- gsub("\\s+", " ", trim(s), perl=TRUE)
    }
    fws <- regexpr(split, s, perl=TRUE)
    
    if (.first_iteration) {
      string_width <- indent + nchar(s)
    } else {
      string_width <- offset + nchar(s)
    }
    if (string_width > width) {
      # if not everything fits on one line
      .first_iteration <- FALSE
      if (FULL_FORCE ||
         ((fws == -1 || fws >= (width - string_width)) && FORCE)) {
        # if no whitespace or first word too long and force break
        # cut through the middle of a word
        pat1 <- paste0("^.{", width - offset - indent, "}(?=.+)")
        pat2 <- paste0("(?<=^.{", width - offset - indent, "}).+")
        leading_string <- regmatches(s, regexpr(pat1, s, perl=TRUE))
        trailing_string <- regmatches(s, regexpr(pat2, s, perl=TRUE)) 
        s <- paste0(indent_string, leading_string, offset_string,
                    linebreak(s=trailing_string, width=width, indent=0,
                              offset=offset, split=split, FORCE=FORCE, 
                              FULL_FORCE=FULL_FORCE))
      } else if ((fws == -1 || fws >= (width - offset + indent)) && !FORCE) {
        # if no whitespace or first word too long and NO force break
        # stop right here
        stop("Can't break in the middle of a word. Use the force!")
      } else {
        # break the line
        s_split <- unlist(strsplit(s, split))
        s_cum <- cumsum(nchar(s_split) + 1)
        leading_string <- 
          paste0(s_split[s_cum < width - offset - indent],
                 ifelse(split == " ", "", split), collapse=split)
        trailing_string <- 
          paste0(s_split[s_cum >= width - offset - indent], collapse=split)
        s <- paste0(indent_string, leading_string, offset_string,
                    linebreak(s=trailing_string, width=width, indent=0,
                              offset=offset, split=split, FORCE=FORCE, FULL_FORCE=FULL_FORCE))
      }
    } else {
      # if everything fits on one line go with the string + indent
      paste0(indent_string, s)
    }
  }, s, width, offset, abs(indent), indent_string, split,
             FORCE, FULL_FORCE, USE.NAMES=FALSE)
  unlist(ans)
}

