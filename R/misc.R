#' Plot the class inheritance graph
#' 
#' This function is taken from the help page for \code{\link[methods]{classesToAM}}.
#' 
#' @param classes A character vector of class names.
#' @param subclasses A logical flag. Include all known subclasses.
#' @param \dots Further arguments.
#' @return Plots a directed graph of class inheritance, and returns it invisibly
#' @export
#' @examples
#' \dontrun{
#' plotInheritance(getClasses("package:Rentrez"))
#' }
plotInheritance <- function (classes, subclasses = FALSE, ...) {
  if (!require("Rgraphviz", quietly=TRUE))
    stop("Only implemented if Rgraphviz is available")
  mm <- classesToAM(classes, subclasses)
  classes <- rownames(mm)
  rownames(mm) <- colnames(mm)
  graph <- new("graphAM", mm, "directed", ...)
  plot(graph)
  cat("Key:\n", paste0(abbreviate(classes), " = ", classes, ", "),
      sep = "", fill = TRUE)
  invisible(graph)
}

