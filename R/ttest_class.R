#' An S4 class for storing results of Bayesian t-test results.
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Raw data for the tested group.
#' @examples
#' summary(`ttest_class`): prints summary of the fit.
#'
#' compare(`ttest_class`, fit2 = `ttest_class`): prints difference/equality of the first group against the second group. You can also provide the rope parameter.
#'
#' compare(`ttest_class`, mu = `numeric`): prints difference/equality of the first group against a mean value. You can also provide the rope parameter.
#'
#' compare(`ttest_class`, mu = `numeric`, sigma = `numeric`): prints difference/equality of the first group against a normal distribution provided with mean value and standard deviation. Note here that sigma is use only in the Cohens d calculation. You can also provide the rope parameter.
#'
#' plot_difference(`ttest_class`, fit2 = `ttest_class`): a visualization of the difference between the first group and the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_difference(`ttest_class`, mu = `numeric`): a visualization of the difference between the first group and a constant value or a normal distribution with mean value mu. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_comparison(`ttest_class`, fit2 = `ttest_class`): plots density for the first and the second group.
#'
#' plot_comparison(`ttest_class`, mu = `numeric`): plots density for the first group and a mean value in case second group is defined as a normal distribution or as a constant.
#'
#' compare_distributions(`ttest_class`, fit2 = `ttest_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, mu = `numeric`): draws samples from distribution of the first group and compares them against a mean value. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, mu = `numeric`, sigma = `numeric`): draws samples from distribution of the first group and compares them against samples from a normal distribution with a defined mean value and variance. You can also provide the rope parameter.
#'
#' plot_distributions(`ttest_class`, fit2 = `ttest_class`): a visualization of the distribution for the first group and the distribution for the second group.
#'
#' plot_distributions(`ttest_class`, mu = `numeric`): a visualization of the distribution for the first group and a constant value.
#'
#' plot_distributions(`ttest_class`, mu = `numeric`, sigma = `numeric`): a visualization of the distribution for the first group and the normal distribution defined with a mean value and standard deviation.
#'
#' plot_distributions_difference(`ttest_class`, fit2 = `ttest_class`): a visualization of the difference between the distribution of the first group and the distribution of the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_distributions_difference(`ttest_class`, mu = `numeric`): a visualization of the difference between the distribution of the first group and a constant value. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_distributions_difference(`ttest_class`, mu = `numeric`, sigma = `numeric`): a visualization of the difference between the distribution of the first group and the normal distribution defined with a mean value and standard deviation. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_fit(`ttest_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' traceplot(`ttest_class`): traceplot for main fitted model parameters.
#'
#' @exportClass ttest_class
ttest_class <- setClass(
  "ttest_class",
  slots = c(
    extract  = "list",
    fit = "stanfit",
    data = "numeric"
  ),
  contains = "b_results"
)


#' @exportMethod summary
setMethod(f = "summary", signature(object = "ttest_class"), definition = function(object) {
  # get means
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)
  nu <- mean(object@extract$nu)

  # print
  cat(sprintf("mu: %.2f\n", mu))
  cat(sprintf("sigma: %.2f\n", sigma))
  cat(sprintf("nu: %.2f\n", nu))
})


#' @title compare
#' @description \code{compare} prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#' @rdname ttest_class-compare
#' @aliases compare,ANY-method
setMethod(f = "compare", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare function are invalid, compare(ttest_class, fit2 = ttest_class), compare(fit2 = ttest_class, mu = numeric), or compare(fit2 = ttest_class, mu = numeric, sigma = numeric) is required! You can also pass the rope parameter, e.g. compare(ttest_class, fit2 = ttest_class, rope = numeric)."

  if (is.null(arguments)) {
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
  mu1 <- object@extract$mu
  sigma1 <- mean(object@extract$sigma)
  n <- length(mu1)

  # second group data
  mu2 <- NULL
  sigma2 <- 0
  if (!is.null(arguments$fit2)) {
    # provided another fit
    fit2 <- arguments$fit2
    mu2 <- fit2@extract$mu
    sigma2 <- mean(fit2@extract$sigma)
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    mu2 <- arguments$mu;

    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
    }
  } else {
    warning(wrong_arguments)
    return()
  }

  shared_difference(mu1, mu2, rope)

  diff <- mean(mu1) - mean(mu2)

  cohens_d <- diff / sqrt((n*sigma1^2 + n*sigma2^2) / (n + n - 2));
  cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
})


#' @title plot_difference
#' @description \code{plot_difference} a visualization of the difference of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#' @rdname ttest_class-plot_difference
#' @aliases plot_difference,ANY-method
setMethod(f = "plot_difference", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_difference function are invalid, plot_difference(ttest_class, ttest_class) or plot_difference(ttest_class, numeric) is required! You can also pass the rope and the bins (number of bins in the histogram) parameters, e.g. plot_difference(ttest_class, fit2 = ttest_class, rope = numeric, bins = numeric)."

  if (is.null(arguments)) {
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
  mu1 <- object@extract$mu

  # second group data
  mu2 <- NULL
  if (!is.null(arguments$fit2)) {
    # provided another fit
    fit2 <- arguments$fit2
    mu2 <- fit2@extract$mu
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    mu2 <- arguments$mu;
  } else {
    warning(wrong_arguments)
    return()
  }

  # bins in the histogram
  bins <- 30
  if (!is.null(arguments$bins)) {
    bins <- arguments$bins
  }

  # call plot difference shared function from shared plots
  shared_plot_difference(mu1, mu2, rope, bins)
})


#' @title plot_comparison
#' @description \code{plot_comparison} plots density for the first group and density for the second group, or a mean value in case second group is defined as a normal distribution or as a constant.
#' @rdname ttest_class-plot_comparison
#' @aliases plot_comparison,ANY-method
setMethod(f = "plot_comparison", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_comparison function are invalid, plot_comparison(ttest_class, ttest_class) or plot_comparison(ttest_class, numeric) is required!"

  if (is.null(arguments)) {
    warning(wrong_arguments)
    return()
  }

  # first group data
  mu1 <- object@extract$mu
  df1 <- data.frame(value = mu1)

  # second group data
  df2 <- NULL
  if (!is.null(arguments$fit2)) {
    # provided another fit
    fit2 <- arguments$fit2
    mu2 <- fit2@extract$mu
    df2 <- data.frame(value = mu2)
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    mu2 <- arguments$mu;
  } else {
    warning(wrong_arguments)
    return()
  }

  # plot
  graph <- ggplot() +
    geom_density(data = df1, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
    theme_minimal() +
    xlab("value")

  if (!is.null(df2)) {
    graph <- graph +
      geom_density(data = df2, aes(x = value), fill = "#ff4e3f", alpha = 0.4, color = NA)
  } else {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = mu2, xend = mu2, y = 0, yend = y_max[2] * 1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", mu2), x = mu2, y = y_max[2] * 1.08), size = 4)
  }

  x_min <- min(mu1, mu2)
  x_max <- max(mu1, mu2)
  diff <- x_max - x_min

  x_min <- x_min - (0.1 * diff)
  x_max <- x_max + (0.1 * diff)

  graph <- graph + xlim(x_min, x_max)

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group, against a mean value, or against samples from a normal distribution with a defined mean value and variance.
#' @rdname ttest_class-compare_distributions
#' @aliases compare_distributions,ANY-method
setMethod(f = "compare_distributions", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(ttest_class, fit2 = ttest_class), compare_distributions(ttest_class, mu = numeric), or compare_distributions(ttest_class, mu = numeric, sigma = numeric) is required! You can also pass the rope parameter, e.g. compare_distributions(ttest_class, fit2 = ttest_class, rope = numeric)."

  if (is.null(arguments)) {
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
  n <- 100000
  nu <- mean(object@extract$nu)
  mu1 <- mean(object@extract$mu)
  sigma1 <- mean(object@extract$sigma)
  y1 <- rt.scaled(n, df = nu, mean = mu1, sd = sigma1)

  # second group data
  y2 <- NULL
  sigma2 <- 0
  if (!is.null(arguments$fit2)) {
    # provided another fit
    fit2 <- arguments$fit2
    mu2 <- mean(fit2@extract$mu)
    sigma2 <- mean(fit2@extract$sigma)

    y2 <- rt.scaled(n, df = nu, mean = mu2, sd = sigma2)
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    mu2 <- arguments$mu;

    if (!is.null(arguments$sigma))
      sigma2 <- arguments$sigma;

    y2 <- rnorm(n, mu2, sigma2)
  } else {
    warning(wrong_arguments)
    return()
  }

  shared_difference(y1, y2, rope)

  diff <- mean(y1) - mean(y2)

  cohens_d <- diff / sqrt((n*sigma1^2 + n*sigma2^2) / (n + n - 2));
  cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
})


#' @title plot_distributions
#' @description \code{plot_distributions} visualizes distributions underlying tested groups.
#' @rdname ttest_class-plot_distributions
#' @aliases plot_distributions,ANY-method
setMethod(f = "plot_distributions", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions function are invalid, plot_distributions(ttest_class, fit2 = ttest_class), plot_distributions(ttest_class, mu = numeric), or plot_distributions(ttest_class, mu = numeric, sigma = numeric) is required!"

  if (is.null(arguments)) {
    warning(wrong_arguments)
    return()
  }

  # first group data
  n <- 10000
  nu <- mean(object@extract$nu)
  y1_mu <- mean(object@extract$mu)
  y1_sigma <- mean(object@extract$sigma)

  # get x range
  x_min <- y1_mu - 4 * y1_sigma
  x_max <- y1_mu + 4 * y1_sigma

  # second group data
  y2_plot <- NULL
  if (!is.null(arguments$fit2)) {
    # provided another fit
    fit2 <- arguments$fit2
    y2_mu <- mean(fit2@extract$mu)
    y2_sigma <- mean(fit2@extract$sigma)

    x_min <- min(x_min, y2_mu - 4 * y2_sigma)
    x_max <- max(x_max, y2_mu + 4 * y2_sigma)

    y2_plot <- stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y2_mu, sd = y2_sigma), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    y2_mu <- arguments$mu;

    if (!is.null(arguments$sigma)) {
      y2_sigma <- arguments$sigma;

      x_min <- min(x_min, y2_mu - 4 * y2_sigma)
      x_max <- max(x_max, y2_mu + 4 * y2_sigma)

      y2_plot <- stat_function(fun = dnorm, n = n, args = list(mean = y2_mu, sd = y2_sigma), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
    } else {
      x_min <- min(x_min, y2_mu)
      x_max <- max(x_max, y2_mu)
    }

  } else {
    warning(wrong_arguments)
    return()
  }

  # plot
  df_x <- data.frame(value = c(x_min, x_max))

  graph <- ggplot(data = df_x, aes(x = value)) +
    stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y1_mu, sd = y1_sigma), geom = 'area', fill = '#3182bd', alpha = 0.4) +
    y2_plot +
    theme_minimal() +
    xlab("value") +
    ylab("density")

  if (is.null(y2_plot)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = y2_mu, xend = y2_mu, y = 0, yend = y_max[2] * 1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", y2_mu), x = y2_mu, y = y_max[2] * 1.08), size = 4)
  }

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the distribution or a constant value for the second group.
#' @rdname ttest_class-plot_distributions_difference
#' @aliases plot_distributions_difference,ANY-method
setMethod(f = "plot_distributions_difference", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(ttest_class, fit2 = ttest_class), plot_distributions_difference(ttest_class, mu = numeric), or plot_distributions_difference(ttest_class, mu = numeric, sigma = numeric) is required! You can also pass the rope and the bins (number of bins in the histogram) parameters, e.g. plot_distributions_difference(ttest_class, fit2 = ttest_class, rope = numeric, bins = numeric)."

  if (is.null(arguments)) {
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
  n <- 100000
  nu <- mean(object@extract$nu)
  y1 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu), sd = mean(object@extract$sigma))

  # second group data
  y2 <- NULL
  if (!is.null(arguments$fit2)) {
    # provided another fit
    fit2 <- arguments$fit2
    y2 <- rt.scaled(n, df = nu, mean = mean(fit2@extract$mu), sd = mean(fit2@extract$sigma))
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    mu2 <- arguments$mu;
    sigma2 <- 0

    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
    }

    y2 <- rnorm(n, mu2, sigma2)
  } else {
    warning(wrong_arguments)
    return()
  }

  # bins in the histogram
  bins <- 30
  if (!is.null(arguments$bins)) {
    bins <- arguments$bins
  }

  # call plot difference shared function from shared plots
  shared_plot_difference(y1, y2, rope, bins)
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @rdname ttest_class-plot_fit
#' @aliases plot_fit,ANY-method
setMethod(f = "plot_fit", signature(object = "ttest_class"), definition = function(object) {
  n <- 10000
  df_data <- data.frame(value = object@data)

  nu <- mean(object@extract$nu)
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)

  # get x range
  x_min <- mu - 4 * sigma
  x_max <- mu + 4 * sigma

  df_x <- data.frame(x = c(x_min, x_max))

  graph <- ggplot(data = df_x) +
    geom_density(data = df_data, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
    stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = mu, sd = sigma), colour = "#3182bd", size = 1) +
    xlab("value") +
    theme_minimal() +
    xlim(x_min, x_max)

  return(graph)
})


#' @title traceplot
#' @description \code{traceplot} traceplot for main fitted model parameters.
#' @rdname ttest_class-traceplot
#' @aliases traceplot,ANY-method
setMethod(f = "traceplot", signature(object = "ttest_class"), definition = function(object) {
  rstan::traceplot(object@fit, pars = c("mu", "sigma", "nu"), inc_warmup = TRUE)
})


### Helper functions
# prepare rope
prepare_rope <- function(rope) {
  # rope is NULL
  if (is.null(rope)) {
    return(NULL)
  }

  # validity check for rope
  if (length(rope) > 2) {
    warning("You provided more than two values for the ROPE interval! Rope value was thus set to 0.")
    return(NULL)
  }
  else if (!is.null(rope) && length(rope) == 1 && rope < 0) {
    warning("When a single number is provided for the ROPE interval it should be positive or 0! Rope value was thus set to 0.")
    return(NULL)
  }

  # if rope as as single number cast it to a list with 2 elements
  if (length(rope) == 1) {
    rope[2] <- rope[1]
    rope[1] <- -rope[1]
  }

  # order ascending
  rope <- sort(rope)

  # return
  return(rope)
}
