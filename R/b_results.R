#' @title b_results
#' @description Parent S4 class for declaring shared function generics.

setClass("b_results")


#' @title compare
#' @rdname b_results-compare
#' @exportMethod compare
setGeneric(name = "compare", function(object, ...) standardGeneric("compare"))


#' @title plot_difference
#' @rdname b_results-plot_difference
#' @exportMethod plot_difference
setGeneric(name = "plot_difference", function(object, ...) standardGeneric("plot_difference"))


#' @title plot_samples
#' @rdname b_results-plot_samples
#' @exportMethod plot_samples
setGeneric(name = "plot_samples", function(object, ...) standardGeneric("plot_samples"))


#' @title compare_distributions
#' @rdname b_results-compare_distributions
#' @exportMethod compare_distributions
setGeneric(name = "compare_distributions", function(object, ...) standardGeneric("compare_distributions"))


#' @title plot_distributions
#' @rdname b_results-plot_distributions
#' @exportMethod plot_distributions
setGeneric(name = "plot_distributions", function(object, ...) standardGeneric("plot_distributions"))


#' @title plot_distributions_difference
#' @rdname b_results-plot_distributions_difference
#' @exportMethod plot_distributions_difference
setGeneric(name = "plot_distributions_difference", function(object, ...) standardGeneric("plot_distributions_difference"))


#' @title plot_fit
#' @rdname b_results-plot_fit
#' @exportMethod plot_fit
setGeneric(name = "plot_fit", function(object) standardGeneric("plot_fit"))


#' @title traceplot
#' @rdname b_results-traceplot
#' @exportMethod traceplot
setGeneric(name = "traceplot", function(object) standardGeneric("traceplot"))
