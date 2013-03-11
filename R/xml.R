#' @importFrom XML xpathSApply
#' @importFrom XML xmlValue
#' @importFrom XML xmlName
#' @importFrom XML xmlGetAttr
#' @export
xvalue <- function(xdoc, path, alt = NA_character_, as = 'character') {
  v <- xpathSApply(xdoc, path, xmlValue) %||% alt
  set_type(v, as)
}

#' @export
xname <- function(xdoc, path, alt = NA_character_, as = 'character') {
  n <- xpathSApply(xdoc, path, xmlName) %||% alt
  set_type(n, as)
}

#' @export
xattr <- function(xdoc, path, name, alt = NA_character_, as = 'character') {
  a <- xpathSApply(xdoc, path, xmlGetAttr, name=name) %||% alt
  set_type(a, as)
}


set_type <- function(x, as) {
  f <- match.fun(paste0('as.', as))
  if (!is.null(x)) {
    f(x)
  } else {
    x
  }
  
}


