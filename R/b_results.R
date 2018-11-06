#' An S4 class for declaring shared generic functions.

setClass("b_results")

#' @title plot_difference
#' @rdname b_results-plot_difference
#' @exportMethod plot_difference
setGeneric(name = "plot_difference", function(object, ...) standardGeneric("plot_difference"))
