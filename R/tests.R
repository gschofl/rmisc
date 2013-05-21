#' @include ops.R
#' @importFrom assertthat on_failure
#' @importFrom assertthat "on_failure<-"
#' @importFrom RSQLite dbGetInfo
NULL

#' Check if a database has specified tables
#'
#' @param con a connection object
#' @param tables a character vector of table names
#' @export
has_tables <- function(con, tbl) {
  assert_that(is(con, "SQLiteConnection"))
  all(tbl %in% dbListTables(con))
}
on_failure(has_tables) <- function(call, env) {
  tbl <- paste0(eval(call$tbl, env), collapse = ", ")
  dbName <- dbGetInfo(eval(call$con, env))$dbname
  paste0("Missing table(s) ", tbl, " in database ", sQuote(dbName))
}


#' @export
#' @rdname has_tables
"%has_tables%" <- has_tables


#' Is empty?
#' 
#' @param x A vector.
#' @export
is_empty <- function (x) {
  if (is.null(x) || length(x) == 0)
    return(TRUE)
  else
    vapply(x, function(x) length(x)==0, logical(1), USE.NAMES=FALSE) | !nzchar(x)
}


#' All empty?
#' 
#' @usage all_empty(x)
#' @param x An object
#' @export
all_empty <- compose(all, is_empty)


#' @export
not.na <- Negate(is.na)


#' Is FALSE?
#' 
#' @usage isFALSE(x) 
#' @param x An object
#' 
#' @export
isFALSE <- function (x) {
  identical(FALSE, x)
}


#' Not in?
#' 
#' @usage x %ni% table
#' @param x A vector.
#' @param table A vector.
#' @export
"%ni%" <- Negate(`%in%`)


#' Is NULL?
#' 
#' Like \code{\link{is.null}} but tests also for
#' \code{NULL} entries in lists
#' 
#' @param x An R object
#' @export
is_null <- function(x) {
  if (is.null(x))
    return(TRUE)
  else if (length(x) == 0)
    return(FALSE)
  else 
    vapply(x, is.null, logical(1), USE.NAMES=FALSE)
}


#' Not NULL?
#' 
#' Like \code{\link{!is.null}} but tests also for
#' \code{NULL} entries in lists
#' 
#' @usage x not_null(x)
#' @param x An R object
#' 
#' @export
not_null <- Negate(is_null)

