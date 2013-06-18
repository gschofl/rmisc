#' @include ops.R
#' @importFrom assertthat on_failure
#' @importFrom assertthat "on_failure<-"
#' @importFrom RSQLite dbGetInfo
NULL


#' Is an object of length one?
#'
#' Which elements in a list are of length one?
#' Are all elements in a list of length one?
#'
#' @param x object to test
#' @family tests
#' @rdname is.scalar
#' @export
#' @examples
#' is.scalar(1)
#' is.scalar(NULL)
is.scalar <- function(x) {
  length(x) == 1L
}
on_failure(is.scalar) <- function(call, env) {
  paste0(deparse(call$x), " does not have length one.")
}

#' @rdname is.scalar
#' @export
#' @examples
#' are_scalar(list(1,2,3))
#' are_scalar(c(1,2,3))
are_scalar <- function(x) {
  assert_that(!is.null(x))
  vapply(x, is.scalar, FUN.VALUE=logical(1), USE.NAMES=FALSE)
}

#' @rdname is.scalar
#' @export
#' @examples
#' all_scalar(list("a", "b", c("c", "d")))
all_scalar <- function(x) all(are_scalar(x))
on_failure(all_scalar) <- function(call, env) {
  paste0("Not all elements in ", deparse(call$x), " are of length one.")
}


#' Is an object empty?
#'
#' Which elements in a list are empty?
#' Are all elements in a list of empty?
#' 
#' @param x object to test
#' @family tests
#' @rdname is.empty
#' @export
#' @examples
#' is.empty(NULL)
#' is.empty(numeric())
#' is.empty(list())
#' is.empty("")
is.empty <- function(x) {
  is.null(x) || length(x) == 0L || ( is.scalar(x) && !nzchar(x) )
}
on_failure(is.empty) <- function(call, env) {
  paste0(deparse(call$x), " is not empty.")
}

#' @rdname is.empty
#' @export
#' @examples
#' are_empty(list(1,NULL,3,NA))
are_empty <- function (x) {
    vapply(x, function (x) is.null(x) || length(x) == 0L,
           FUN.VALUE=logical(1), USE.NAMES=FALSE) | !nzchar(x)
}

#' @rdname is.empty
#' @export
#' @examples
#' all_empty(NULL)
#' all_empty(list(NULL, NULL, character()))
all_empty <- function(x) all(are_empty(x))
on_failure(all_empty) <- function(call, env) {
  paste0("Not all elements in ", deparse(call$x), " are empty.")
}


#' Check if a SQLite database has specified tables
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


#' On a UNIX-like system, test if an external dependency is available
#' 
#' @param cmd The command to test.
#' @param msg Additional message if the test fails.
#' @family tests
#' @export
has_command <- function (cmd, msg = "") {
  assert_that(is.string(cmd))
  if (.Platform$OS.type == "unix") {
    exec <- paste("which", cmd)
    cmd %in% basename(suppressWarnings(system(exec, intern=TRUE)))
  } else {
    warning("No test for the presence of", cmd,
            "implemented for a", .Platform$OS.type, "platform")
    TRUE
  }
}
on_failure(has_command) <- function(call, env) {
  paste0("Dependency ", sQuote(eval(call$cmd, env)), " is not installed\n",
         eval(call$msg, env))
}


#' Which elements in a list are NULL?
#'
#' @param x object to test
#' @rdname are_null
#' @export
#' @examples
#' are_null(list(1,NULL,3))
are_null <- function(x) {
  assert_that(is.list(x))
  vapply(x, is.null, FUN.VALUE=logical(1), USE.NAMES=FALSE)
}


#' Which elements in a list are TRUE?
#'
#' @param x object to test
#' @export
#' @examples
#' are_true(list(FALSE,TRUE,TRUE))
are_true <- function(x) {
  assert_that(is.list(x))
  vapply(x, isTRUE, FUN.VALUE=logical(1), USE.NAMES=FALSE)
}


#' Which elements in a list are FALSE?
#'
#' @param x object to test
#' @export
#' @examples
#' are_false(list(FALSE,TRUE,TRUE))
are_false <- function(x) {
  assert_that(is.list(x))
  vapply(x, function (x) identical(x, FALSE), FUN.VALUE=logical(1), USE.NAMES=FALSE)
}


#' Not in?
#' 
#' @usage x %ni% table
#' @param x A vector.
#' @param table A vector.
#' @export
"%ni%" <- Negate(`%in%`)


