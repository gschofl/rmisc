##' Wrap alignment
##' 
##' @param seq1 First sequence.
##' @param ... More sequences
##' @param prefix Character vector of text going before the alignment
##' @param suffix Text going after the alignment
##' @param start Numeric vector of starting positions of the sequences.
##' @param reverse Any reversed sequences
##' @param sep Space between prefix/suffix and position numbering.
##' 
##' @export
wrapAln <- function (seq1,
                     ..., 
                     prefix=c(""),
                     suffix=c(""),
                     start=c(1),
                     reverse=c(FALSE),
                     sep=2)
{
  
  seqs <- c(seq1, list(...))
  lseqs <- sapply(seqs, nchar)
  
  if (!length(unique(lseqs)) == 1L)
    stop("Sequences are of different length")
  
  pref_width <- max(sapply(prefix, nchar))
  aln_start_width <- aln_end_width <- nchar(as.character(unique(lseqs)))
  suf_width <- max(sapply(suffix, nchar))
  offset <- pref_width + sep + aln_start_width + 1 + 1 + aln_end_width + sep + suf_width  
  
  # break up sequences  
  s <- linebreak(seqs, getOption("width") - offset, FULL_FORCE=TRUE)
  s <- str_split(s, "\n")  
  seq_widths <- nchar(s[[1L]])
  max_seq_width <- max(seq_widths)
  
  seq_starts <-
    mapply(function (start, rev) {
      x <- Reduce("+", seq_widths, init=start, right=rev, accumulate=TRUE)
      x <- x[-which.max(x)]
      x
    }, start=start, rev=reverse, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  new_starts <- 
    mapply( function (s, rev) if (rev) s[length(s) - 1] - 1 else s[2] - 1,
            s=seq_starts, rev=reverse)
  
  seq_ends <-
    mapply( function (start, rev) {
      x <- Reduce("+", seq_widths, init=start, right=rev, accumulate=TRUE)
      x <- x[-which.max(x)]
    }, start=new_starts, rev=reverse, SIMPLIFY=FALSE, USE.NAMES=FALSE)  
  
  tmp <- seq_ends[reverse]
  seq_ends[reverse] <- seq_starts[reverse]
  seq_starts[reverse] <- tmp
  
  pasteAln <- function(prefix, seq_starts, s, seq_ends, suffix) {
    seq_starts[is.empty(seq_starts)] <- ""
    seq_ends[is.empty(seq_ends)] <- ""
    paste0(str_pad(prefix, pref_width, side="right"),
           blanks(sep),
           str_pad(as.character(seq_starts), aln_start_width, side="left"),
           blanks(1),
           str_pad(s, max_seq_width, side="right"),
           blanks(1),
           str_pad(as.character(seq_ends), aln_start_width, side="left"),
           blanks(sep),
           str_pad(suffix, suf_width, side="right"))
  }
  
  s <- mapply(pasteAln, prefix=prefix, seq_starts=seq_starts, s=s,
              seq_ends=seq_ends, suffix=suffix,
              SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  s <- paste0(do.call(function (...) paste(..., sep="\n"), s),
              collapse="\n\n")
  s
}
