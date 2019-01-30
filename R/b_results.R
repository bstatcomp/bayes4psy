#' @title b_results
#' @description Parent S4 class for declaring shared function generics.
#' summary(`success_rate_class`): prints summary od the fit.
setClass("b_results")


#' @title compare
#' @description \code{compare} prints difference between two groups.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters.
#' @rdname b_results
#' @exportMethod compare
setGeneric(name = "compare", function(object, ...) standardGeneric("compare"))


#' @title plot_difference
#' @description \code{plot_difference} plots difference between two groups.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters.
#' @rdname b_results
#' @exportMethod plot_difference
setGeneric(name = "plot_difference", function(object, ...) standardGeneric("plot_difference"))


#' @title plot_samples
#' @description \code{plot_samples} plots samples for one or two groups.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters.
#' @rdname b_results
#' @exportMethod plot_samples
setGeneric(name = "plot_samples", function(object, ...) standardGeneric("plot_samples"))


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group,
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters.
#' @rdname b_results
#' @exportMethod compare_distributions
setGeneric(name = "compare_distributions", function(object, ...) standardGeneric("compare_distributions"))


#' @title plot_distributions
#' @description \code{plot_distributions} visualizes distributions underlying tested groups.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters.
#' @rdname b_results
#' @exportMethod plot_distributions
setGeneric(name = "plot_distributions", function(object, ...) standardGeneric("plot_distributions"))


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the distribution or a constant value for the second group.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters.
#' @rdname b_results
#' @exportMethod plot_distributions_difference
setGeneric(name = "plot_distributions_difference", function(object, ...) standardGeneric("plot_distributions_difference"))


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object S4 class object from EasyBayes library.
#' @rdname b_results
#' @exportMethod plot_fit
setGeneric(name = "plot_fit", function(object) standardGeneric("plot_fit"))


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object S4 class object from EasyBayes library.
#' @rdname b_results
#' @exportMethod plot_trace
setGeneric(name = "plot_trace", function(object) standardGeneric("plot_trace"))
