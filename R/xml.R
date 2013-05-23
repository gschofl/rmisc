#' @importFrom XML xpathSApply
#' @importFrom XML xmlValue
#' @importFrom XML xmlName
#' @importFrom XML xmlGetAttr
NULL

#' @export
xvalue <- function(xdoc, path, alt = NA_character_, as = 'character',
                   fun = function (x) x, ...)
{
  v <- xpathSApply(xdoc, path, fun%.%xmlValue, ...) %||% alt
  set_type(v, as)
}

#' @export
xname <- function(xdoc, path, alt = NA_character_, as = 'character',
                  fun = function (x) x, ...)
{
  n <- xpathSApply(xdoc, path, fun%.%xmlName, ...) %||% alt
  set_type(n, as)
}

#' @export
xattr <- function(xdoc, path, name, alt = NA_character_, as = 'character',
                  fun = function (x) x, ...)
{
  a <- xpathSApply(xdoc, path, fun%.%xmlGetAttr, name=name, ...) %||% alt
  set_type(a, as)
}

#' @export
xsize <- function(xdoc, path, ...)
{
  length(xpathSApply(xdoc, path, ...))
}

set_type <- function(x, as)
{
  f <- match.fun(paste0('as.', as))
  if (!is.null(x)) {
    f(x)
  } else {
    x
  }
}


