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
  assert_that(is.list(a))
  assert_that(is.list(b))
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


#' Number of unique elements in a vector.
#' 
#' A wrapper around \code{length(unique(x))}
#' 
#' @param x vector
#' @param ... passed to \code{\link{unique}}
#' @export
nunique <- function(x, ...) {
  if (is.factor(x)) {
    length(levels(x))
  } else {
    length(unique(x, ...))
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


#' Execute code in an temporarily different environment.
#'
#' \itemize{
#'   \item \code{with_cpp11}: temporarily set -std=c++11.
#'   \item \code{with_localtime}: temporarily change locale time.
#'   }
#'   
#' @param new New value.
#' @param code Code to execute.
#' @name with
#' @examples
#' \dontrun{
#' rmisc <- as.package("~/R/Projects/Devel/rmisc")
#' with_cpp11(devtools::build(rmisc))
#' }
NULL


#' @rdname with
#' @export
with_cpp11 <- function(code) {
  old <- Sys.getenv("PKG_CXXFLAGS", names=TRUE)
  Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
  on.exit(do.call("Sys.setenv", as.list(old)))
  force(code)
}


#' @rdname with
#' @export
with_localtime <- function(new, code) {
  old <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", new)
  on.exit(Sys.setlocale("LC_TIME", old))
  force(code)
}

