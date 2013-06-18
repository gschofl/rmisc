#' @importFrom XML xpathApply
#' @importFrom XML xmlValue
#' @importFrom XML xmlName
#' @importFrom XML xmlGetAttr
NULL

#' @export
xvalue <- function(doc, path, alt = NA_character_, as = 'character',
                   fun = function (x) x, ...)
{
  fun <- compose(fun, xmlValue)
  v <- unlist(xpathApply(doc, path, fun, ...)) %||% alt
  set_type(v, as)
}


#' @export
xname <- function(doc, path, alt = NA_character_, as = 'character',
                  fun = function (x) x, ...)
{
  fun <- compose(fun, xmlName)
  n <- unlist(xpathApply(doc, path, fun, ...)) %||% alt
  set_type(n, as)
}


#' @export
xattr <- function(doc, path, name, alt = NA_character_, as = 'character',
                  fun = function (x) x, ...)
{
  fun <- compose(fun, xmlGetAttr)
  a <- unlist(xpathApply(doc, path, fun, name=name, ...)) %||% alt
  set_type(a, as)
}


#' @export
xsize <- function(doc, path, ...)
{
  length(xpathApply(doc, path, ...))
}


#' @export
xset <- function(doc, path, ...) {
  xpathApply(doc, path, fun = NULL, ...)
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


