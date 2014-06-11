#' Register a caching environment
#' 
#' Generate a \code{cache} object, with \code{set}, \code{get},
#' \code{exists}, \code{ls}, and \code{rm} methods.
#' 
#' @export
#' @examples
#' cache <- new_cache()
#' cache$set("a", 1:10)
#' cache$set("b", list(1,2,3,4))
#' cache$ls()
#' cache$get("a")
#' cache$exists("b")
#' cache$exists("c")
#' cache$rm()     
new_cache <- function () {
  
  cache <- NULL
  
  set_val <- function (key, value) {
    assert_that(is.string(key))
    assign(key, value, cache)
  }
  get_val <- function (key) {
    assert_that(is.string(key))
    get(key, cache, inherits = FALSE)
  }
  key_exists <- function (key) {
    assert_that(is.string(key))
    exists(key, cache, inherits = FALSE)
  }
  .list <- function () {
    ls(cache)
  }
  .remove <- function () {
    cache <<- new.env(hash = TRUE, emptyenv())
  }
  
  .remove()
  
  structure(
    list(
      set = set_val, 
      get = get_val,
      exists = key_exists,
      ls = .list,
      rm = .remove
    ),
    class = "cache"
  )
}

#' @export
print.cache <- function (object) {
  lo <- length(object$ls())
  showme <- sprintf("A datacache containing %s object%s.",
                    lo, if (lo == 1) "" else "s")
  cat(showme, sep='\n')
}

