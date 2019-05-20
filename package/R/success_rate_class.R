#' @title success_rate_class
#' @import dplyr ggplot2
#' @description An S4 class for storing results of successes (true/false) Bayesian model.
#'
#' \strong{Functions}
#'
#' summary(`success_rate_class`): prints summary od the fit.
#'
#' print(`success_rate_class`): prints a more detailed summary of the fit
#'
#' show(`success_rate_class`): prints a more detailed summary of the fit.
#'
#' get_samples(`success_rate_class`): returns a dataframe with values of fitted parameters.
#'
#' get_subject_samples(`success_rate_class`): returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#'
#' compare_samples(`success_rate_class`, fit2=`success_rate_class`): prints difference in successfulness of two groups. You can also provide the rope parameter.
#'
#' plot_samples_difference(`success_rate_class`, fit2=`success_rate_class`): a visualization of the difference between two groups. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_samples(`success_rate_class`): plots density for the first samples.
#'
#' plot_samples(`success_rate_class`, fit2=`success_rate_class`): plots density for the first and the second group samples.
#'
#' compare_distributions(`success_rate_class`, fit2=`success_rate_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#'
#' plot_distributions(`success_rate_class`): a visualization of the fitted distribution.
#'
#' plot_distributions(`success_rate_class`, fit2=`success_rate_class`): a visualization of the distribution for two fits.
#'
#' plot_distributions_difference(`success_rate_class`, fit2=`success_rate_class`): a visualization of the difference between the distribution of the first group and the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_fit(`success_rate_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_trace(`success_rate_class`): traceplot for main fitted model parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
success_rate_class <- setClass(
  "success_rate_class",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)


#' @title summary
#' @description \code{summary} prints summary of the Bayesian success rate fit.
#' @param object success_rate_class object.
#' @exportMethod summary
setMethod(f="summary", signature(object="success_rate_class"), definition=function(object) {
  # get means
  p <- mean(object@extract$p0)

  # hdi
  p_hdi <- mcmc_hdi(object@extract$p0)

  # print
  cat(sprintf("Success rate:\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              p, mcmcse::mcse(object@extract$p0)$se, p_hdi[1], p_hdi[2]))
})


#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian success rate fit.
#' @param object success_rate_class object.
#' @exportMethod show
setMethod(f="show", signature(object="success_rate_class"), definition=function(object) {
  # print
  show(object@fit)
})


#' @title get_samples
#' @description \code{get_samples} returns a dataframe with values of fitted parameters.
#' @param object success_rate_class object.
#' @rdname success_rate_class-get_samples
#' @aliases get_samples_success_rate_class
setMethod(f="get_samples", signature(object="success_rate_class"), definition=function(object) {
  df <- data.frame(p=object@extract$p0,
                   tau=object@extract$tau)

  return(df)
})


#' @title get_subject_samples
#' @description \code{get_subject_samples} returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#' @param object success_rate_class object.
#' @rdname success_rate_class-get_subject_samples
#' @aliases get_subject_samples_success_rate_class
setMethod(f="get_subject_samples", signature(object="success_rate_class"), definition=function(object) {
  df <- data.frame(p=numeric(), tau=numeric(), subject=numeric())

  n <- length(unique(object@data$s))

  for (i in 1:n) {
    df_subject <- data.frame(p = object@extract$p[,i],
                             tau = object@extract$tau[,i],
                             subject = i)

    df <- rbind(df, df_subject)
  }

  return(df)
})


#' @title compare_samples
#' @description \code{compare_samples} prints difference in successfulness between two groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, rope - region of practical equivalence.
#' @rdname success_rate_class-compare_samples
#' @aliases compare_samples_success_rate
setMethod(f="compare_samples", signature(object="success_rate_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_samples function are invalid, compare_samples(success_rate_class, fit2=success_rate_class) is required! You can also provide the rope parameter, e.g. compare_samples(success_rate_class, fit2=success_rate_class, rope=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # first group data
  y1 <- object@extract$p0

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y2 <- fit2@extract$p0

    shared_difference(y1=y1, y2=y2, rope=rope)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_samples_difference
#' @description \code{plot_samples_difference} a visualization of the difference between two groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname success_rate_class-plot_samples_difference
#' @aliases plot_samples_difference_success_rate
setMethod(f="plot_samples_difference", signature(object="success_rate_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_samples_difference function are invalid, plot_samples_difference(success_rate_class, fit2=success_rate_class) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_samples_difference(success_rate_class, fit2=success_rate_class, rope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # first group data
  y1 <- object@extract$p0

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y2 <- fit2@extract$p0

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)
    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_samples
#' @description \code{plot_samples} plots density of the samples, or the first and the second group samples.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object.
#' @rdname success_rate_class-plot_samples
#' @aliases plot_samples_success_rate
setMethod(f="plot_samples", signature(object="success_rate_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  group <- value <- NULL

  # first group data
  df <- data.frame(value=object@extract$p0, group="1")

  # second group data
  df2 <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }

      df <- rbind(df, data.frame(value=fit2@extract$p0, group="2"))
    }
  }

  # limits
  x_min <- min(df$value)
  x_max <- max(df$value)
  diff <- x_max - x_min
  x_min <- x_min - 0.1*diff
  x_max <- x_max + 0.1*diff

  # plot
  graph <- ggplot() +
    geom_density(data=df, aes(x=value, fill=group), alpha=0.4, color=NA) +
    scale_fill_manual(values=c("#3182bd", "#ff4e3f")) +
    xlab("value") +
    xlim(x_min, x_max) +
    theme(legend.position="none")

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object.
#' @rdname success_rate_class-compare_distributions
#' @aliases compare_distributions_success_rate
setMethod(f="compare_distributions", signature(object="success_rate_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(success_rate_class, fit2=success_rate_class) is required!)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  n <- 100000

  # first group data
  p0 <- mean(object@extract$p0)
  tau <- mean(object@extract$tau)
  y1 <- stats::rbeta(n, p0*tau, (1 - p0)*tau)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    p0 <- mean(fit2@extract$p0)
    tau <- mean(fit2@extract$tau)
    y2 <- stats::rbeta(n, p0*tau, (1 - p0)*tau)

    # calculate
    y1_smaller <- round(sum(y1 < y2) / n, 2)
    y1_greater <- round(sum(y1 > y2) / n, 2)
    equal <- 1 - y1_smaller - y1_greater

    # print
    cat(sprintf("Probabilities:\n  - Group 1 < Group 2: %.2f", y1_smaller))
    cat(sprintf("\n  - Group 1 > Group 2: %.2f", y1_greater))
    cat(sprintf("\n  - Equal: %.2f\n", equal))
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the fitted distribution, for one or two fits.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object.
#' @rdname success_rate_class-plot_distributions
#' @aliases plot_distributions_success_rate
setMethod(f="plot_distributions", signature(object="success_rate_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- y <- y_min <- NULL

  n <- 10000

  # first group data
  p0 <- mean(object@extract$p0)
  tau <- mean(object@extract$tau)
  alpha1 <- p0*tau
  beta1 <- (1 - p0)*tau

  # second group data
  group2_plot <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      p0 <- mean(fit2@extract$p0)
      tau <- mean(fit2@extract$tau)
      alpha2 <- p0*tau
      beta2 <- (1 - p0)*tau

      group2_plot <- stat_function(fun=stats::dbeta, n=n, args=list(shape1=alpha2, shape2=beta2), geom="area", fill="#ff4e3f", alpha=0.4)
    }
  }

  # plot
  df_x <- data.frame(value=c(0, 1))

  graph <- ggplot(data=df_x, aes(x=value)) +
    stat_function(fun=stats::dbeta, n=n, args=list(shape1=alpha1, shape2=beta1), geom="area", fill="#3182bd", alpha=0.4) +
    group2_plot +
    xlab("probability") +
    ylab("density")

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the second group.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object.
#' @rdname success_rate_class-plot_distributions_difference
#' @aliases plot_distributions_difference_success_rate
setMethod(f="plot_distributions_difference", signature(object="success_rate_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  x <- value <- variable <- NULL

  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(success_rate_class, fit2=success_rate_class) is required! You can also provide the rope and bins (number of bins in the histogram) parameter, e.g. plot_distributions_difference(success_rate_class, fit2=success_rate_class, rope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  n <- 100000

  # results
  df <- data.frame()

  # first group data
  p0 <- mean(object@extract$p0)
  tau <- mean(object@extract$tau)
  y1 <- stats::rbeta(n, p0*tau, (1 - p0)*tau)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    p0 <- mean(fit2@extract$p0)
    tau <- mean(fit2@extract$tau)
    y2 <- stats::rbeta(n, p0*tau, (1 - p0)*tau)

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)
    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object success_rate_class object.
#' @param ... subjects - plot fits on a subject level (default = FALSE).
#' @rdname success_rate_class-plot_fit
#' @aliases plot_fit_success_rate
setMethod(f="plot_fit", signature(object="success_rate_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  variable<-value<-NULL

  arguments <- list(...)

  # plot on a subject level?
  subjects <- FALSE
  if (!is.null(arguments$subjects)) {
    subjects <- arguments$subjects
  }

  df_data <- data.frame(value=object@data$r, variable=object@data$s)

  if (!subjects) {
    mean_p <- mean(df_data$value)

    df_fit <- data.frame(value=object@extract$p0)

    graph <- ggplot() +
      geom_vline(xintercept=mean_p, color="#ff4e3f") +
      geom_density(data=df_fit, aes(x=value), fill="#3182bd", alpha=0.4, color=NA) +
      xlim(0, 1) +
      xlab("success rate")
  } else {
    df_data <- df_data %>% group_by(variable) %>% summarize(value=mean(value))

    df_fit <- object@extract$p
    colnames(df_fit) <- seq(1:ncol(df_fit))
    df_fit <- reshape::melt(as.data.frame(df_fit), id=NULL)

    # ncol
    n_col <- ceiling(sqrt(nrow(df_data)))

    # density per subject
    graph <- ggplot() +
      geom_vline(data=df_data, aes(xintercept=value), color="#ff4e3f") +
      geom_density(data=df_fit, aes(x=value), fill="#3182bd", alpha=0.4, color=NA) +
      xlim(0, 1) +
      facet_wrap(~ variable, ncol=n_col) +
      xlab("success rate")
  }

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object success_rate_class object.
#' @rdname success_rate_class-plot_trace
#' @aliases plot_trace_success_rate
setMethod(f="plot_trace", signature(object="success_rate_class"), definition=function(object) {
  rstan::traceplot(object@fit, pars=c("p"), inc_warmup=TRUE)
})
