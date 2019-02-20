#' @title success_rate_class
#' @import dplyr ggplot2
#' @description An S4 class for storing results of successes (true/false) Bayesian model.
#' summary(`success_rate_class`): prints summary od the fit.
#'
#' print(`success_rate_class`): prints a more detailed summary of the fit
#'
#' show(`success_rate_class`): prints a more detailed summary of the fit.
#'
#' compare(`success_rate_class`, fit2 = `success_rate_class`): prints difference in successfulness of two groups. You can also provide the rope parameter.
#'
#' plot_difference(`success_rate_class`, fit2 = `success_rate_class`): a visualization of the difference between two groups. You can also provide the rope parameter.
#'
#' plot_samples(`success_rate_class`): plots density for the first samples.
#'
#' plot_samples(`success_rate_class`, fit2 = `success_rate_class`): plots density for the first and the second group samples.
#'
#' compare_distributions(`success_rate_class`, fit2 = `success_rate_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#'
#' plot_distributions(`success_rate_class`): a visualization of the distribution for the first group.
#'
#' plot_distributions(`success_rate_class`, fit2 = `success_rate_class`): a visualization of the distribution for the first group and the second group.
#'
#' plot_distributions_difference(`success_rate_class`, fit2 = `success_rate_class`): a visualization of the difference between the distribution of the first group and the second group. You can also provide the rope parameter.
#'
#' plot_fit(`success_rate_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_trace(`success_rate_class`): traceplot for main fitted model parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
#' @exportClass success_rate_class
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
setMethod(f = "summary", signature(object = "success_rate_class"), definition = function(object) {
  # get means
  p <- mean(object@extract$p)

  # hdi
  p_hdi <- mcmc_hdi(object@extract$p)

  # print
  cat(sprintf("Success rate: %.2f, 95%% HDI: [%.2f, %.2f]\n", p, p_hdi[1], p_hdi[2]))
})

#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian success rate fit.
#' @param object success_rate_class object.
#' @exportMethod show
setMethod(f = "show", signature(object = "success_rate_class"), definition = function(object) {
  # print
  show(object@fit)
})

#' @title compare
#' @description \code{compare} prints difference in successfulness between two groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, rope - region of practical equivalence.
#' @rdname success_rate_class-compare
setMethod(f = "compare", signature(object = "success_rate_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare function are invalid, compare(success_rate_class, fit2 = success_rate_class) is required! You can also provide the rope parameter, e.g. compare(success_rate_class, fit2 = success_rate_class, rope = numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope = arguments$rope
  }
  rope <- prepare_rope(rope)

  # first group data
  y1 <- rowMeans(object@extract$p)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y2 <- rowMeans(fit2@extract$p)

    shared_difference(y1 = y1, y2 = y2, rope = rope)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_difference
#' @description \code{plot_difference} a visualization of the difference between two groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname success_rate_class-plot_difference
setMethod(f = "plot_difference", signature(object = "success_rate_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_difference function are invalid, plot_difference(success_rate_class, fit2 = success_rate_class) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_difference(success_rate_class, fit2 = success_rate_class, rope = numeric, bins = numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope = arguments$rope
  }
  rope <- prepare_rope(rope)

  # first group data
  y1 <- rowMeans(object@extract$p)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y2 <- rowMeans(fit2@extract$p)

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph <- shared_plot_difference(y1 = y1, y2 = y2, rope = rope, bins = bins)
    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_samples
#' @description \code{plot_samples} plots density for the first group samples, or the first and the second group samples.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object.
#' @rdname success_rate_class-plot_samples
setMethod(f = "plot_samples", signature(object = "success_rate_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  value=NULL

  # first group data
  df1 <- data.frame(value = rowMeans(object@extract$p))

  # limits
  x_min <- min(df1$value)
  x_max <- max(df1$value)

  # plot
  graph <- ggplot() +
    geom_density(data = df1, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA)

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
      df2 <- data.frame(value = rowMeans(fit2@extract$p))

      # limits
      x_min <- min(x_min, df2$value)
      x_max <- max(x_max, df2$value)

      # plot
      graph <- graph +
        geom_density(data = df2, aes(x = value), fill = "#ff4e3f", alpha = 0.4, color = NA)
    }
  }

  # limits
  diff <- x_max - x_min
  x_min <- x_min - 0.1*diff
  x_max <- x_max + 0.1*diff

  # plot
  graph <- graph +
    xlab("value") +
    xlim(x_min, x_max)

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object.
#' @rdname success_rate_class-compare_distributions
setMethod(f = "compare_distributions", signature(object = "success_rate_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(success_rate_class, fit2 = success_rate_class) is required!)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  n <- 100000

  # first group data
  p1 <- mean(object@extract$p)
  y1 <- stats::rbinom(n, 1, p1)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    p2 <- mean(fit2@extract$p)
    y2 <- stats::rbinom(n, 1, p2)

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
#' @description \code{plot_distributions} a visualization of the distribution for the first group, or the first group and the second group.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object.
#' @rdname success_rate_class-plot_distributions
setMethod(f = "plot_distributions", signature(object = "success_rate_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  value=y=y_min=NULL

  n <- nrow(object@extract$p)
  m <- 10000

  # first group data
  p1 <- mean(object@extract$p)
  y1 <- stats::rbinom(m, n, p1) / n
  mu1 <- mean(y1)
  sd1 <- stats::sd(y1)

  # second group data
  group2_plot <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      p2 <- mean(fit2@extract$p)
      y2 <- stats::rbinom(m, n, p2) / n
      mu2 <- mean(y2)
      sd2 <- stats::sd(y2)

      group2_plot <- stat_function(fun = stats::dnorm, n = m, args = list(mean = mu2, sd = sd2), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
    }
  }

  # plot
  df_x <- data.frame(value = c(0, 1))

  graph <- ggplot(data = df_x, aes(x = value)) +
    stat_function(fun = stats::dnorm, n = m, args = list(mean = mu1, sd = sd1), geom = 'area', fill = '#3182bd', alpha = 0.4) +
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
setMethod(f = "plot_distributions_difference", signature(object = "success_rate_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  x=value=variable=NULL

  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(reaction_time_class, fit2 = reaction_time_class) is required!"

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  n <- 100000

  # first group data
  p1 <- mean(object@extract$p)
  y1 <- stats::rbinom(n, 1, p1)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    p2 <- mean(fit2@extract$p)
    y2 <- stats::rbinom(n, 1, p2)

    # calculate
    y1_smaller <- round(sum(y1 < y2) / n, 2)
    y1_greater <- round(sum(y1 > y2) / n, 2)
    equal <- 1 - y1_smaller - y1_greater

    levels <- c("Group 1 > Group 2", "Equal", "Group 1 < Group 2")
    df <- data.frame(x = c(1, 1, 1),
                     variable = factor(levels, levels = levels),
                     value = c(y1_greater, equal, y1_smaller))

    # plot
    graph <- ggplot(data = df, aes(x = x, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#3182bd", "grey80", "#ff4e3f")) +
      theme(legend.title=element_blank()) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      ylab("Probability") +
      xlim(0.3, 1.7)

    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object success_rate_class object.
#' @rdname success_rate_class-plot_fit
setMethod(f = "plot_fit", signature(object = "success_rate_class"), definition = function(object) {
  # init local varibales for CRAN check
  r=x=subject=variable=value=NULL

  df_data <- data.frame(r = object@data$r, subject = object@data$s)

  df_data <- df_data %>% group_by(subject) %>% summarize(data=mean(r))
  #ddply(df_data, ~ subject, summarise, data=mean(r))

  df_data$fit <- colMeans(object@extract$p)

  # ncol
  n_col <- ceiling(sqrt(nrow(df_data)))

  # melt
  df_data <- reshape::melt(as.data.frame(df_data), id = "subject")

  # density per subject
  graph <- ggplot(df_data, aes(x = variable, y = value, fill = variable)) +
    geom_bar(stat="identity") +
    facet_wrap(~ subject, ncol = n_col) +
    scale_fill_manual(values = c("#3182bd", "#ff4e3f")) +
    ylim(0, 1) +
    theme(legend.title=element_blank())

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object success_rate_class object.
#' @rdname success_rate_class-plot_trace
setMethod(f = "plot_trace", signature(object = "success_rate_class"), definition = function(object) {
  rstan::traceplot(object@fit, pars = c("p"), inc_warmup = TRUE)
})
