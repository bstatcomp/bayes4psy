#' @title b_results
#' @description Parent S4 class for declaring shared function generics.
#'
#' \strong{Functions}
#'
#' summary(`success_rate_class`): prints summary of the fit.
#'
#' print(`success_rate_class`): prints a more detailed summary of the fit.
#'
#' show(`success_rate_class`): prints a more detailed summary of the fit.
setClass("b_results")

#' @title get_samples
#' @description \code{get_samples} returns a dataframe with values of fitted parameters.
#' @param object S4 class object from EasyBayes library.
#' @rdname b_results-get_samples
#' @exportMethod get_samples
setGeneric(name="get_samples", function(object) standardGeneric("get_samples"))

#' @title compare
#' @description \code{compare} prints difference between two groups.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?compare_ttest or ?compare_linear.
#' @rdname b_results-compare
#' @exportMethod compare
setGeneric(name="compare", function(object, ...) standardGeneric("compare"))


#' @title plot_difference
#' @description \code{plot_difference} plots difference between two groups.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_difference_ttest or ?plot_difference_linear.
#' @rdname b_results-plot_difference
#' @exportMethod plot_difference
setGeneric(name="plot_difference", function(object, ...) standardGeneric("plot_difference"))


#' @title plot_samples
#' @description \code{plot_samples} plots samples for one or two groups.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_samples_ttest or ?plot_samples_linear.
#' @rdname b_results-plot_samples
#' @exportMethod plot_samples
setGeneric(name="plot_samples", function(object, ...) standardGeneric("plot_samples"))

#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group,
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?compare_distributions_ttest or ?compare_distributions_linear.
#' @rdname b_results-compare_distributions
#' @exportMethod compare_distributions
setGeneric(name="compare_distributions", function(object, ...) standardGeneric("compare_distributions"))


#' @title plot_distributions
#' @description \code{plot_distributions} visualizes fitted distributions.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_distributions_ttest or ?plot_distributions_linear.
#' @rdname b_results-plot_distributions
#' @exportMethod plot_distributions
setGeneric(name="plot_distributions", function(object, ...) standardGeneric("plot_distributions"))


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the distribution or a constant value for the second group.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_distributions_difference_ttest or ?plot_distributions_difference_linear.
#' @rdname b_results-plot_distributions_difference
#' @exportMethod plot_distributions_difference
setGeneric(name="plot_distributions_difference", function(object, ...) standardGeneric("plot_distributions_difference"))


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object S4 class object from EasyBayes library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_fit_colors or ?plot_fit_linear.
#' @rdname b_results-plot_fit
#' @exportMethod plot_fit
setGeneric(name="plot_fit", function(object, ...) standardGeneric("plot_fit"))


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object S4 class object from EasyBayes library.
#' @rdname b_results-plot_trace
#' @exportMethod plot_trace
setGeneric(name="plot_trace", function(object) standardGeneric("plot_trace"))
