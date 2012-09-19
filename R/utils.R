#' Chain functions
#' 
#' @param x object
#' @param f function
#' 
#' @export
"%@%" <- function(x, f) {
  eval.parent(as.call(append(as.list(substitute(f)), list(x), 1)))
}


#' create blank strings with a given number of characters
#' 
#' @param n A numeric vector
#' 
#' @seealso Examples for \code{\link{regmatches}}
#' @export
blanks <- function(n) {
  vapply(Map(rep.int, rep.int(" ", length(n)), n, USE.NAMES=FALSE),
         paste0, collapse="", character(1))
}


#' @export
merge_list <- function (x, y, ...) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x) 
  i <- is.na(match(names(y), names(x)))
  if (any(i)) {
    x[names(y)[which(i)]] = y[which(i)]
  }
  x
}


#' @export
merge_dups <- function (x) {
  if (is_empty(x))
    return(NULL)
  x_names <- names(x)
  a <- x[!duplicated(x_names)]
  b <- x[duplicated(x_names)]
  modify_list(a, b, "merge")
}


# taken from lattice by Deepayan Sarkar
#' @export
modify_list <- function (a, b, mode=c("replace",  "merge")) {
  stopifnot(is.list(a), is.list(b))
  mode <- match.arg(mode)
  a_names <- names(a)
  for (v in names(b)) {
    a[[v]] <- if (v %in% a_names && is.list(a[[v]]) && is.list(b[[v]])) {
      modify_list(a[[v]], b[[v]])
    } else {
      switch(mode,
             replace=b[[v]],
             merge=unique(c(a[[v]], b[[v]])))
    }
  }
  a
}


# taken from roxygen3 by Hadley Wickham
#' @export
compact <- function (x) {
  null <- vapply(x, is.null, logical(1))
  x[!null]
}


#' @export
compactNA <- function (x) {
   na <- vapply(x, is.na, logical(1))
   x[!na]
}


#' Purge rows containing NAs from a data frame
#' 
#' returns the supplied colums without rows containing NAs as a data frames
#' 
#' @param df data frame
#' @param cols column names
#' 
#' @return data frame
#' 
#' @export
purgeNA <- function (df, cols) {
  if (length(cols) <= 1) {
    stop("Brauche mindestens zwei Spaltennamen um einen Datenrahmen zu bauen")
  }
  if(!all(cols %in% names(df))) {
    stop("Einen oder mehrere der Spaltennamen gibt es nicht im Datenrahmen")
  }
  df <- df[, names(df) %in% cols]
  df <- df[!Reduce("|", lapply(df, is.na)), ]
  return(df)
}


#' benchmark a function
#' 
#' @param f function call
#' @param rep replicate runs
#' 
#' @export
benchmark <- function(f, rep) {
  mean(replicate(rep, system.time(eval(substitute(f))))["elapsed",])
}

