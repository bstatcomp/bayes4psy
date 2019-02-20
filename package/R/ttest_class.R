#' @title ttest_class
#' @import ggplot2 metRology rstan
#' @description An S4 class for storing results of Bayesian t-test results.
#' summary(`ttest_class`): prints summary of the fit.
#'
#' print(`ttest_class`): prints a more detailed summary of the fit
#'
#' show(`ttest_class`): prints a more detailed summary of the fit.
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
#' plot_samples(`ttest_class`): plots density for the first group samples.
#'
#' plot_samples(`ttest_class`, fit2 = `ttest_class`): plots density for the first and the second group samples.
#'
#' plot_samples(`ttest_class`, mu = `numeric`): plots density for the first group samples and a mean value in case second group is defined as a normal distribution or as a constant.
#'
#' compare_distributions(`ttest_class`, fit2 = `ttest_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, mu = `numeric`): draws samples from distribution of the first group and compares them against a mean value. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, mu = `numeric`, sigma = `numeric`): draws samples from distribution of the first group and compares them against samples from a normal distribution with a defined mean value and variance. You can also provide the rope parameter.
#'
#' plot_distributions(`ttest_class`): a visualization of the distribution for the first group.
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
#' plot_trace(`ttest_class`): traceplot for main fitted model parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Raw data for the tested group.
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

#' @title summary
#' @description \code{summary} prints summary of the Bayesian ttest fit.
#' @param object ttest_class object.
#' @exportMethod summary
setMethod(f = "summary", signature(object = "ttest_class"), definition = function(object) {
  # get means
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)
  nu <- mean(object@extract$nu)

  # hdi
  mu_hdi <- mcmc_hdi(object@extract$mu)
  sigma_hdi <- mcmc_hdi(object@extract$sigma)
  nu_hdi <- mcmc_hdi(object@extract$nu)

  # print
  cat(sprintf("mu: %.2f, 95%% HDI: [%.2f, %.2f]\n", mu, mu_hdi[1], mu_hdi[2]))
  cat(sprintf("sigma: %.2f, 95%% HDI: [%.2f, %.2f]\n", sigma, sigma_hdi[1], sigma_hdi[2]))
  cat(sprintf("nu: %.2f, 95%% HDI: [%.2f, %.2f]\n", nu, nu_hdi[1], nu_hdi[2]))
})

#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian ttest fit.
#' @param object ttest_class object.
#' @exportMethod show
setMethod(f = "show", signature(object = "ttest_class"), definition = function(object) {
  # print
  show(object@fit)
})

#' @title compare
#' @description \code{compare} prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, sigma - standard deviation, rope - region of practical equivalence.
#' @rdname ttest_class-compare
setMethod(f = "compare", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare function are invalid, compare(ttest_class, fit2 = ttest_class), compare(fit2 = ttest_class, mu = numeric), or compare(fit2 = ttest_class, mu = numeric, sigma = numeric) is required! You can also provide the rope parameter, e.g. compare(ttest_class, fit2 = ttest_class, rope = numeric)."

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
  y1 <- object@extract$mu
  sigma1 <- mean(object@extract$sigma)
  n <- length(y1)

  # second group data
  y2 <- NULL
  sigma2 <- 0
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y2 <- fit2@extract$mu
    sigma2 <- mean(fit2@extract$sigma)
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    y2 <- arguments$mu;

    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
    }
  } else {
    warning(wrong_arguments)
    return()
  }

  shared_difference(y1 = y1, y2 = y2, rope = rope)

  diff <- mean(y1) - mean(y2)

  cohens_d <- diff / sqrt((n*sigma1^2 + n*sigma2^2) / (n + n - 2));
  cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
})


#' @title plot_difference
#' @description \code{plot_difference} a visualization of the difference of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname ttest_class-plot_difference
setMethod(f = "plot_difference", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_difference function are invalid, plot_difference(ttest_class, fit2 = ttest_class) or plot_difference(ttest_class, mu = numeric) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_difference(ttest_class, fit2 = ttest_class, rope = numeric, bins = numeric)."

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
  y1 <- object@extract$mu

  # second group data
  y2 <- NULL
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y2 <- fit2@extract$mu
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    y2 <- arguments$mu;
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
  graph <- shared_plot_difference(y1 = y1, y2 = y2, rope = rope, bins = bins)
  return(graph)
})


#' @title plot_samples
#' @description \code{plot_samples} plots density for the first group samples, the first and the second group samples, or a mean value in case second group is defined as a normal distribution or as a constant.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value.
#' @rdname ttest_class-plot_samples
setMethod(f = "plot_samples", signature(object = "ttest_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  # first group data
  mu1 <- object@extract$mu
  df1 <- data.frame(value = mu1)

  # plot
  graph <- ggplot() +
    geom_density(data = df1, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
    xlab("value")

  # second group data
  df2 <- NULL
  mu2 <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      mu2 <- fit2@extract$mu
      df2 <- data.frame(value = mu2)
    } else if (!is.null(arguments$mu)) {
      # provided mu and sigma
      mu2 <- arguments$mu;
    }
  }

  if (!is.null(df2)) {
    graph <- graph +
      geom_density(data = df2, aes(x = value), fill = "#ff4e3f", alpha = 0.4, color = NA)
  } else if (!is.null(mu2)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = mu2, xend = mu2, y = 0, yend = y_max[2]*1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", mu2), x = mu2, y = y_max[2]*1.08), size = 4)
  }

  # limits
  x_min <- min(mu1, mu2)
  x_max <- max(mu1, mu2)
  diff <- x_max - x_min

  x_min <- x_min - 0.1*diff
  x_max <- x_max + 0.1*diff

  graph <- graph + xlim(x_min, x_max)

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group, against a mean value, or against samples from a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, sigma - standard deviation, rope - region of practical equivalence.
#' @rdname ttest_class-compare_distributions
setMethod(f = "compare_distributions", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(ttest_class, fit2 = ttest_class), compare_distributions(ttest_class, mu = numeric), or compare_distributions(ttest_class, mu = numeric, sigma = numeric) is required! You can also provide the rope parameter, e.g. compare_distributions(ttest_class, fit2 = ttest_class, rope = numeric)."

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
  n <- 100000
  nu <- mean(object@extract$nu)
  mu1 <- mean(object@extract$mu)
  sigma1 <- mean(object@extract$sigma)
  y1 <- rt.scaled(n, df = nu, mean = mu1, sd = sigma1)

  # second group data
  y2 <- NULL
  sigma2 <- 0
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    mu2 <- mean(fit2@extract$mu)
    sigma2 <- mean(fit2@extract$sigma)

    y2 <- rt.scaled(n, df = nu, mean = mu2, sd = sigma2)
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    mu2 <- arguments$mu;

    if (!is.null(arguments$sigma))
      sigma2 <- arguments$sigma;

    y2 <- stats::rnorm(n, mu2, sigma2)
  } else {
    warning(wrong_arguments)
    return()
  }

  shared_difference(y1 = y1, y2 = y2, rope = rope)

  diff <- mean(y1) - mean(y2)

  cohens_d <- diff / sqrt((n*sigma1^2 + n*sigma2^2) / (n + n - 2));
  cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
})


#' @title plot_distributions
#' @description \code{plot_distributions} visualizes distributions underlying tested groups.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, sigma - standard deviation.
#' @rdname ttest_class-plot_distributions
setMethod(f = "plot_distributions", signature(object = "ttest_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  # first group data
  n <- 10000
  nu <- mean(object@extract$nu)
  y1_mu <- mean(object@extract$mu)
  y1_sigma <- mean(object@extract$sigma)

  # get x range
  x_min <- y1_mu - 4*y1_sigma
  x_max <- y1_mu + 4*y1_sigma

  # second group data
  group2_plot <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      y2_mu <- mean(fit2@extract$mu)
      y2_sigma <- mean(fit2@extract$sigma)

      x_min <- min(x_min, y2_mu - 4*y2_sigma)
      x_max <- max(x_max, y2_mu + 4*y2_sigma)

      group2_plot <- stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y2_mu, sd = y2_sigma), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
    } else if (!is.null(arguments$mu)) {
      # provided mu and sigma
      y2_mu <- arguments$mu;

      if (!is.null(arguments$sigma)) {
        y2_sigma <- arguments$sigma;

        x_min <- min(x_min, y2_mu - 4*y2_sigma)
        x_max <- max(x_max, y2_mu + 4*y2_sigma)

        group2_plot <- stat_function(fun = stats::dnorm, n = n, args = list(mean = y2_mu, sd = y2_sigma), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
      } else {
        x_min <- min(x_min, y2_mu)
        x_max <- max(x_max, y2_mu)
      }
    }
  }

  # plot
  df_x <- data.frame(value = c(x_min, x_max))

  graph <- ggplot(data = df_x, aes(x = value)) +
    stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y1_mu, sd = y1_sigma), geom = 'area', fill = '#3182bd', alpha = 0.4) +
    group2_plot +
    xlab("value") +
    ylab("density")

  if (!is.null(arguments$mu) && is.null(arguments$sigma)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = y2_mu, xend = y2_mu, y = 0, yend = y_max[2]*1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", y2_mu), x = y2_mu, y = y_max[2]*1.08), size = 4)
  }

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the distribution or a constant value for the second group.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, sigma - standard deviation, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname ttest_class-plot_distributions_difference
setMethod(f = "plot_distributions_difference", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(ttest_class, fit2 = ttest_class), plot_distributions_difference(ttest_class, mu = numeric), or plot_distributions_difference(ttest_class, mu = numeric, sigma = numeric) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_distributions_difference(ttest_class, fit2 = ttest_class, rope = numeric, bins = numeric)."

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
  n <- 100000
  nu <- mean(object@extract$nu)
  y1 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu), sd = mean(object@extract$sigma))

  # second group data
  y2 <- NULL
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y2 <- rt.scaled(n, df = nu, mean = mean(fit2@extract$mu), sd = mean(fit2@extract$sigma))
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    mu2 <- arguments$mu;
    sigma2 <- 0

    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
    }

    y2 <- stats::rnorm(n, mu2, sigma2)
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
  graph <- shared_plot_difference(y1 = y1, y2 = y2, rope = rope, bins = bins)
  return(graph)
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object ttest_class object.
#' @rdname ttest_class-plot_fit
setMethod(f = "plot_fit", signature(object = "ttest_class"), definition = function(object) {
  # init local varibales for CRAN check
  value <- NULL

  n <- 10000
  df_data <- data.frame(value = object@data)

  nu <- mean(object@extract$nu)
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)

  # get x range
  x_min <- mu - 4*sigma
  x_max <- mu + 4*sigma

  df_x <- data.frame(x = c(x_min, x_max))

  graph <- ggplot(data = df_x) +
    geom_density(data = df_data, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
    stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = mu, sd = sigma), colour = "#3182bd", size = 1) +
    xlab("value") +
    xlim(x_min, x_max)

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object ttest_class object.
#' @rdname ttest_class-plot_trace
setMethod(f = "plot_trace", signature(object = "ttest_class"), definition = function(object) {
  traceplot(object@fit, pars = c("mu", "sigma"), inc_warmup = TRUE)
})
