setOldClass('list')

#' Collection-class
#'
#' @importFrom IRanges elementType
#' @importFrom IRanges as.list
#' @name Collection-class
#' @rdname Collection-class
#' @exportClass Collection
setClass('Collection',
         contains = 'list',
         representation(
           elementType = 'character',
           shared = 'environment'),
         prototype(
           elementType = NA_character_,
           shared = new.env(parent=emptyenv())))

#' @export
setMethod("elementType", "Collection", function (x) x@elementType)

#' @export
setMethod("as.list", "Collection", function (x) x@.Data)

#' @export
#' @docType methods
setGeneric('shared', function(x, ...) standardGeneric('shared'))
setMethod("shared", "Collection", function(x, value = NULL) {
  if (is.null(value))
    x@shared
  else
    tryCatch(get(value, envir=x@shared, inherits=FALSE),
             error = function (e) NULL)
})

#' @export
setMethod("[", "Collection", function(x, i, j, ..., drop) {
  new(class(x), callNextMethod(), elementType = elementType(x),
      shared = shared(x))
})

#' @export
setMethod("[[", "Collection", function(x, i, j, ...) {
  callNextMethod()
})


#' Create a constructor function for classes extending \code{list}
#'  
#' @param Class Name of the collection class.
#'
#' @importFrom assertthat assert_that is.string
#' @export
collectionConstructor <- function (Class) {
  assert_that(is.string(Class))
  assert_that(extends(Class, "Collection"))
  
  function (..., shared = new.env(parent=emptyenv())) {
    listData <- list(...)
    elementType <- elementType(new(Class))
    if (length(listData) == 0L) {
      new(Class, list(new(elementType)), elementType=elementType, shared=shared)
    }
    else {
      if (length(listData) == 1L && is.list(listData[[1L]])) 
        listData <- listData[[1L]]
      if (!all(vapply(listData, is, elementType, FUN.VALUE=logical(1L)))) 
        stop("All elements in '...' must be '", elementType,"' objects")
      
      new(Class, listData, elementType=elementType, shared=shared)
    }
  }
}


#' @export
collectionValidator <- function (Class) {
  assert_that(is.string(Class))

  function (object) {
    errors <- character()
    elementType <- elementType(object)
    elem_of_class <- vapply(as.list(object), is, elementType, FUN.VALUE=logical(1L))
    if (!all(elem_of_class)) {
      msg <- paste0("All elements in a '", Class ,"' must be of type '",
                    elemType, "'.")
      errors <- c(errors, msg)
    }
    
    if (length(errors) == 0L)
      TRUE
    else
      errors
  }
}


#' @importFrom assertthat has_args
#' @export
collectionShower <- function (showFun, numOfElements = 6, linesPerElement = NULL) {
  assert_that(is.function(showFun), has_args(showFun, c('x', 'width', 'ellipsis')))
  assert_that(is.numeric(numOfElements), length(numOfElements) == 1)
  
  showElements <- function (index, elems, lPerEl = NULL, ellipsis = ' ... ' ) {
    index_string <- paste0('[[', index, ']] ')
    if (is.null(lPerEl)) {
      width <- Inf
    }
    else {
      indent <- nchar(index_string) + nchar(ellipsis) + 2L*lPerEl + 1L
      width <- lPerEl*getOption("width") - indent
    }
    object_string <- unlist(Map(showFun, x=elems, width=width, ellipsis=ellipsis))
    sprintf("%s%s", index_string, linebreak(object_string, indent = -nchar(index_string),
                                            offset=1L, FORCE=TRUE))
  }
  
  ##' @param x A Collection instance
  ##' @param nOfEl how many Collection elements do we show as head and tail.
  ##' @param lPerEl how many lines per collection element to we want to show
  ##' before we ellipsize.
  function (x,
            nOfEl = getOption('numOfElements') %||% numOfElements,
            lPerEl = getOption('linesPerElement') %||% linesPerElement)
  {
    data <- as.list(x)
    ll <- length(data)
    cat(sprintf("A %s instance of length %s\n", sQuote(class(x)), ll), sep="")
    
    if (ll == 0L) {
      showme <- ''
    }
    else if (ll > 2*nOfEl) {
      head_index <- seq_len(nOfEl)
      head <- data[head_index]
      showHead <- showElements(head_index, head, lPerEl)
      tail_index <- seq.int(to=ll, length.out = min(nOfEl, ll))
      tail <- data[tail_index]
      showTail <- showElements(tail_index, tail, lPerEl)
      showme <- c(showHead, '...', showTail)
    }
    else {
      showme <- showElements(seq_along(data), data, lPerEl)
    }
    cat(showme, sep="\n")
  }
}

