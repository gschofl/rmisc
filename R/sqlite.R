#' @importFrom assertthat assert_that is.string is.readable not_empty noNA
#' @importFrom RSQLite SQLite dbConnect dbGetQuery dbDisconnect dbCommit
#' @importFrom RSQLite dbListTables dbBeginTransaction dbSendPreparedQuery
NULL

#' Create an SQLite database.
#' 
#' @param dbName Path to an SQLite database.
#' @param dbSchema SQL schema for setting up the db.
#' 
#' @export
db_create <- function(dbName, dbSchema = "")
{
  assert_that(is.string(dbSchema))
  message('Creating new database ', sQuote(basename(dbName)))
  if (file.exists(dbName)) {
    unlink(dbName)
  }
  
  con <- dbConnect(SQLite(), dbname = dbName)
  
  sql <- compact(trim(strsplit(dbSchema, ";\n")[[1L]]), are_empty)
  if (!all_empty(sql)) {
    tryCatch(lapply(sql, dbGetQuery, conn = con),
             error = function (e) {
               message(e)
             })
  }

  con
}


#' Connect to an existing SQLite database.
#' 
#' @param dbName Path to an SQLite database.
#' @param message Message if db does not exits
#' 
#' @export
db_connect <- function(dbName, message = "") {
  if (!file.exists(dbName))
    stop("Database ", sQuote(basename(dbName)),
         " does not exist.\n", message, call.=FALSE)
  dbConnect(SQLite(), dbname=dbName)
}


#' Disconnect from an SQLite database.
#' 
#' @param ... connection objects.
#' 
#' @export
db_disconnect <- function(...) {
  lapply(list(...), dbDisconnect)
}


#' Query an SQLite database.
#' 
#' @param con a connection object.
#' @param sql an SQL statemant
#' @param j
#' 
#' @export
db_query <- function(con, sql, j=NA)
{
  assert_that(is(con, "SQLiteConnection"))
  assert_that(is.string(sql), noNA(sql))
  data <- dbGetQuery(con, sql)
  if (is.na(j))
    return(data)
  if (nrow(data) == 0)
    return(character(0))
  else
    return(data[[j]])
}

#' Count rows in a db table
#' 
#' @param con a connection object.
#' @param tbl name of table in database.
#' 
#' @export
db_count <- function(con, tbl) {
  assert_that(con %has_tables% tbl)
  sql <- paste0("SELECT count(*) FROM ", tbl)
  db_query(con, sql, 1)
}


#' @export
db_bulk_insert <- function(con, tbl, df) {
  sql <- sprintf("INSERT INTO %s VALUES (%s)", tbl,
                 paste0("$", names(df), collapse=", "))
  dbBeginTransaction(con)
  dbSendPreparedQuery(con, sql, df)
  dbCommit(con)
}
