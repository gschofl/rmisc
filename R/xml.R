#' @importFrom XML xpathApply xmlValue xmlName xmlGetAttr
NULL

#' @export
xvalue <- function(doc, path, alt = NA_character_, as = 'character',
                   fun = NULL, ...)
{
  fun <- Compose(fun, xmlValue)
  v <- unlist(xpathApply(doc, path, fun, ...)) %||% alt
  set_mode(v, as)
}


#' @export
xname <- function(doc, path, alt = NA_character_, as = 'character',
                  fun = NULL, ...)
{
  fun <- Compose(fun, xmlName)
  n <- unlist(xpathApply(doc, path, fun, ...)) %||% alt
  set_mode(n, as)
}


#' @export
xattr <- function(doc, path, name, alt = NA_character_, as = 'character',
                  fun = NULL, ...)
{
  fun <- Compose(fun, xmlGetAttr)
  a <- unlist(xpathApply(doc, path, fun, name=name, ...)) %||% alt
  set_mode(a, as)
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


set_mode <- function(x, as) {
  f <- match.fun(paste0('as.', as))
  if (!is.null(x)) f(x) else x
}


