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
  if (all_empty(x))
    return(NULL)
  x_names <- names(x)
  a <- x[!duplicated(x_names)]
  b <- x[duplicated(x_names)]
  modify_list(a, b, "merge")
}


## pinched from the lattice package.
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

 
#' Filter entries from a list.
#' 
#' @param x A list.
#' @param filter A function to filter entries. (default \code{is.null})
#' @export
compact <- function (x, filter = "is.null") {
  filter <- match.fun(filter)
  x[!vapply(x, filter, logical(1), USE.NAMES=FALSE)]
}


#' Filter NA entries from a list.
#' 
#' @param x A vector.
#' @export
compactNA <- Curry("compact", filter = "is.na")


#' Filter empty entries from a list.
#' 
#' @param x A vector.
#' @export
compactAll <- Curry("compact", filter = "is_empty")


## pinched from the plyr package.
#' Calculate the number of unique values.
#' 
#' @param x vector
#' @export
nunique <- function(x) {
  if (is.factor(x)) {
    length(levels(x))
  } else {
    length(unique(x))
  }
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


#' Benchmark a function
#' 
#' Call \code{\link{system.time}} n times and average
#' over the replicate runs.
#' 
#' @param FUN A function call
#' @param n Number of replicate runs
#' @return An object of class \code{\link{proc_time}}
#' @export
benchmark <- function(FUN, n = 1) {
  r <- replicate(n, system.time(eval(substitute(FUN))))
  structure(rowMeans(r), class = "proc_time")
}

