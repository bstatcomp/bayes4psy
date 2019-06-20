#' @title b_results
#' @description Parent S4 class for declaring shared function generics.
setClass("b_results")


#' @title get_parameters
#' @description \code{get_parameters} returns a dataframe with values of fitted parameters.
#' @param object S4 class object from bayes4psy library.
#' @rdname b_results-get_parameters
#' @exportMethod get_parameters
setGeneric(name="get_parameters", function(object) standardGeneric("get_parameters"))


#' @title get_subject_parameters
#' @description \code{get_subject_parameters} returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#' @param object S4 class object from bayes4psy library.
#' @rdname b_results-get_subject_parameters
#' @exportMethod get_subject_parameters
setGeneric(name="get_subject_parameters", function(object) standardGeneric("get_subject_parameters"))


#' @title compare_means
#' @description \code{compare_means} prints difference between means of two or multiple fits.
#' @param object S4 class object from bayes4psy library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?compare_means_ttest or ?compare_means_linear.
#' @rdname b_results-compare_means
#' @exportMethod compare_means
setGeneric(name="compare_means", function(object, ...) standardGeneric("compare_means"))


#' @title plot_means_difference
#' @description \code{plot_means_difference} plots difference between means of two or multiple fits.
#' @param object S4 class object from bayes4psy library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_means_difference_ttest or ?plot_means_difference_linear.
#' @rdname b_results-plot_means_difference
#' @exportMethod plot_means_difference
setGeneric(name="plot_means_difference", function(object, ...) standardGeneric("plot_means_difference"))


#' @title plot_means
#' @description \code{plot_means} plots means for one or multiple fits.
#' @param object S4 class object from bayes4psy library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_means_ttest or ?plot_means_linear.
#' @rdname b_results-plot_means
#' @exportMethod plot_means
setGeneric(name="plot_means", function(object, ...) standardGeneric("plot_means"))


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first fit and compares them against samples drawn from the distribution of the second fit, or against samples from multiple fits.
#' @param object S4 class object from bayes4psy library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?compare_distributions_ttest or ?compare_distributions_linear.
#' @rdname b_results-compare_distributions
#' @exportMethod compare_distributions
setGeneric(name="compare_distributions", function(object, ...) standardGeneric("compare_distributions"))


#' @title plot_distributions
#' @description \code{plot_distributions} visualizes fitted distributions.
#' @param object S4 class object from bayes4psy library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_distributions_ttest or ?plot_distributions_linear.
#' @rdname b_results-plot_distributions
#' @exportMethod plot_distributions
setGeneric(name="plot_distributions", function(object, ...) standardGeneric("plot_distributions"))


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distributions of two or more fits.
#' @param object S4 class object from bayes4psy library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_distributions_difference_ttest or ?plot_distributions_difference_linear.
#' @rdname b_results-plot_distributions_difference
#' @exportMethod plot_distributions_difference
setGeneric(name="plot_distributions_difference", function(object, ...) standardGeneric("plot_distributions_difference"))


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object S4 class object from bayes4psy library.
#' @param ... see documentation for specific class for the description of available parameters, e.g. ?plot_fit_colors or ?plot_fit_linear.
#' @rdname b_results-plot_fit
#' @exportMethod plot_fit
setGeneric(name="plot_fit", function(object, ...) standardGeneric("plot_fit"))


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object S4 class object from bayes4psy library.
#' @rdname b_results-plot_trace
#' @exportMethod plot_trace
setGeneric(name="plot_trace", function(object) standardGeneric("plot_trace"))
