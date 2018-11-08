# validity check for this class
ttest_results_check <- function(object) {
  errors <- character()

  # rope as a single number < 0
  if (length(object@rope) > 2) {
    msg <- "You provided more than two values for the ROPE interval!"
  }
  else if (!is.null(object@rope) && object@rope[1] == object@rope[2] && object@rope[1] < 0) {
    msg <- "When a single number is provided for the ROPE interval it should be positive!"
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

# set class unions to allow NULL values
setClassUnion("numeric_or_null", c("numeric", "NULL"))
setClassUnion("list_or_null", c("list", "NULL"))

#' An S4 class for storing results of Bayesian t-test results.
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot mu Optional variable: mean value for comparison with first group.
#' @slot sigma Optional variable: standard deviation for comparison with first group, used in combination with `mu` parameter.
#' @slot rope Optional variable: rope interval used in comparisons.
#' @slot data1 Raw data for the first tested group.
#' @slot data2 Optional variable: raw data for the second tested group.
#' @examples
#' summary(`ttest_results`): prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#'
#' compare(`ttest_results`): prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#'
#' plot_difference(`ttest_results`): a visualization of the difference of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#'
#' plot_comparison(`ttest_results`): plots density for the first group and density for the second group, or a mean value in case second group is defined as a normal distribution or as a constant.
#'
#' compare_distributions(`ttest_results`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group, against a mean value, or against samples from a normal distribution with a defined mean value and variance.
#'
#' plot_distributions(`ttest_results`): a visualization of the distribution for the first group and the distribution or a constant value for the second group.
#'
#' plot_distributions_difference(`ttest_results`): a visualization of the difference between the distribution of the first group and the distribution or a constant value for the second group.
#'
#' plot_fit(`ttest_results`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' traceplot(`ttest_results`): traceplot for main fitted model parameters.
#'
#' @exportClass ttest_results
ttest_results <- setClass(
  "ttest_results",
  slots = c(
    extract  = "list",
    fit = "stanfit",
    mu = "numeric_or_null",
    sigma = "numeric_or_null",
    rope = "numeric_or_null",
    data1 = "numeric",
    data2 = "numeric_or_null"
  ),
  validity = ttest_results_check,
  contains = "b_results"
)


#' @exportMethod summary
setMethod(f = "summary", signature(object = "ttest_results"), definition = function(object) {
  compare_groups(object)
})


#' @title compare
#' @description \code{compare} prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#' @rdname ttest_results-compare
#' @aliases compare,ANY-method
setMethod(f = "compare", signature(object = "ttest_results"), definition = function(object) {
  compare_groups(object)
})


#' @title plot_difference
#' @description \code{plot_difference} a visualization of the difference of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#' @rdname ttest_results-plot_difference
#' @aliases plot_difference,ANY-method
setMethod(f = "plot_difference", signature(object = "ttest_results"), definition = function(object, ..., bins = 30) {
  # prepare rope
  rope <- prepare_rope(object@rope)

  # first group data
  mu1 <- object@extract$mu[, 1]

  # second group data
  mu2 <- NULL
  if (ncol(object@extract$mu) == 2) {
    mu2 <- object@extract$mu[, 2]
  } else if (!is.null(object@mu)) {
    mu2 <- object@mu
  }

  # call plot difference shared function from shared plots
  shared_plot_difference(mu1, mu2, rope)
})


#' @title plot_comparison
#' @description \code{plot_comparison} plots density for the first group and density for the second group, or a mean value in case second group is defined as a normal distribution or as a constant.
#' @rdname ttest_results-plot_comparison
#' @aliases plot_comparison,ANY-method
setMethod(f = "plot_comparison", signature(object = "ttest_results"), definition = function(object) {
  # get samples
  mu1 <- object@extract$mu[, 1]
  df1 <- data.frame(value = mu1)

  mu2 <- NULL
  # is second group calculated by stan fit?
  if (ncol(object@extract$mu) == 2) {
    mu2 <- object@extract$mu[, 2]
    df2 <- data.frame(value = mu2)
  # second group is given as a normal distribution or a constant
  } else {
    mu2 <- object@mu
  }

  # plot
  graph <- ggplot() +
    geom_density(data = df1, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
    theme_minimal() +
    xlab("Value")

  if (is.null(object@mu)) {
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
#' @rdname ttest_results-compare_distributions
#' @aliases compare_distributions,ANY-method
setMethod(f = "compare_distributions", signature(object = "ttest_results"), definition = function(object) {
  # prepare rope
  rope <- prepare_rope(object@rope)

  # first group data
  n <- 100000
  nu <- mean(object@extract$nu)
  y1 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu[, 1]), sd = mean(object@extract$sigma[, 1]))

  # second group data
  y2 <- NULL
  if (ncol(object@extract$mu) == 2) {
    y2 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu[, 2]), sd = mean(object@extract$sigma[, 2]))
  } else if (!is.null(object@mu)) {
    if (is.null(object@sigma))
      object@sigma = 0;

    y2 <- rnorm(n, object@mu, object@sigma)
  }

  shared_difference(y1, y2, rope)
})


#' @title plot_distributions
#' @description \code{plot_distributions} visualizes distributions underlying tested groups.
#' @rdname ttest_results-plot_distributions
#' @aliases plot_distributions,ANY-method
setMethod(f = "plot_distributions", signature(object = "ttest_results"), definition = function(object) {
  # first group data
  n <- 10000
  nu <- mean(object@extract$nu)
  y1_mu <- mean(object@extract$mu[, 1])
  y1_sigma <- mean(object@extract$sigma[, 1])

  # get x range
  x_min <- y1_mu - 4 * y1_sigma
  x_max <- y1_mu + 4 * y1_sigma

  y2_plot <- NULL
  # is second group calculated by stan fit?
  if (ncol(object@extract$mu) == 2) {
    y2_mu <- mean(object@extract$mu[, 2])
    y2_sigma <- mean(object@extract$sigma[, 2])

    x_min <- min(x_min, y2_mu - 4 * y2_sigma)
    x_max <- max(x_max, y2_mu + 4 * y2_sigma)

    y2_plot <- stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y2_mu, sd = y2_sigma), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
  # second group is given as a normal distribution
  } else if (!is.null(object@mu) && !is.null(object@sigma)) {
    y2_mu <- object@mu
    y2_sigma <- object@sigma

    x_min <- min(x_min, y2_mu - 4 * y2_sigma)
    x_max <- max(x_max, y2_mu + 4 * y2_sigma)

    y2_plot <- stat_function(fun = dnorm, n = n, args = list(mean = y2_mu, sd = y2_sigma), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
  # second group is just a constant number, part 1 (because we need max y for vertical line)
  } else if (!is.null(object@mu)) {
    y2_mu <- object@mu

    x_min <- min(x_min, y2_mu)
    x_max <- max(x_max, y2_mu)
  }

  # plot
  df_x <- data.frame(value = c(x_min, x_max))

  graph <- ggplot(data = df_x, aes(x = value)) +
    stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y1_mu, sd = y1_sigma), geom = 'area', fill = '#3182bd', alpha = 0.4) +
    y2_plot +
    theme_minimal() +
    xlab("Value")

  if (!is.null(object@mu) && is.null(object@sigma)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = y2_mu, xend = y2_mu, y = 0, yend = y_max[2] * 1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", y2_mu), x = y2_mu, y = y_max[2] * 1.08), size = 4)
  }

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the distribution or a constant value for the second group.
#' @rdname ttest_results-plot_distributions_difference
#' @aliases plot_distributions_difference,ANY-method
setMethod(f = "plot_distributions_difference", signature(object = "ttest_results"), definition = function(object) {
  # prepare rope
  rope <- prepare_rope(object@rope)

  # first group data
  n <- 100000
  nu <- mean(object@extract$nu)
  y1 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu[, 1]), sd = mean(object@extract$sigma[, 1]))

  # second group data
  y2 <- NULL
  if (ncol(object@extract$mu) == 2) {
    y2 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu[, 2]), sd = mean(object@extract$sigma[, 2]))
  } else if (!is.null(object@mu)) {
    if (is.null(object@sigma))
      object@sigma = 0;

    y2 <- rnorm(n, mean(object@mu), mean(object@sigma))
  }

  shared_plot_difference(y1, y2, rope)
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @rdname ttest_results-plot_fit
#' @aliases plot_fit,ANY-method
setMethod(f = "plot_fit", signature(object = "ttest_results"), definition = function(object) {
  n <- 10000
  df_data1 <- data.frame(value = object@data1)

  nu <- mean(object@extract$nu)
  y1_mu <- mean(object@extract$mu[, 1])
  y1_sigma <- mean(object@extract$sigma[, 1])

  # get x range
  x_min <- y1_mu - 4 * y1_sigma
  x_max <- y1_mu + 4 * y1_sigma

  density2 <- NULL
  function2 <- NULL
  if (!is.null(object@data2)) {
    df_data2 <- data.frame(value = object@data2)
    y2_mu <- mean(object@extract$mu[, 2])
    y2_sigma <- mean(object@extract$sigma[, 2])

    x_min <- min(x_min, y2_mu - 4 * y2_sigma)
    x_max <- max(x_max, y2_mu + 4 * y2_sigma)

    density2 <- geom_density(data = df_data2, aes(x = value), fill = "#ff4e3f", alpha = 0.4, color = NA)
    function2 <- stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y2_mu, sd = y2_sigma), colour = "#ff4e3f", size = 1)
  }

  df_x <- data.frame(x = c(x_min, x_max))

  graph <- ggplot(data = df_x) +
    geom_density(data = df_data1, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
    stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y1_mu, sd = y1_sigma), colour = "#3182bd", size = 1) +
    density2 +
    function2 +
    xlab("Value") +
    theme_minimal() +
    xlim(x_min, x_max)

  return(graph)
})


#' @title traceplot
#' @description \code{traceplot} traceplot for main fitted model parameters.
#' @rdname ttest_results-traceplot
#' @aliases traceplot,ANY-method
setMethod(f = "traceplot", signature(object = "ttest_results"), definition = function(object) {
  rstan::traceplot(object@fit, pars = c("mu", "sigma", "nu"), inc_warmup = TRUE)
})


### Helper functions
# comparison function
compare_groups <- function(object) {
  # prepare rope
  rope <- prepare_rope(object@rope)

  # first group data
  mu1 <- object@extract$mu[, 1]
  sigma1 <- mean(object@extract$sigma[, 1])
  n <- length(mu1)

  # second group data
  mu2 <- NULL
  if (ncol(object@extract$mu) == 2) {
    mu2 <- object@extract$mu[, 2]
    sigma2 <- mean(object@extract$sigma[, 2])
  } else if (!is.null(object@mu)) {
    mu2 <- rep(object@mu, n)
    sigma2 <- 0
  }

  shared_difference(mu1, mu2, rope)

  diff <- mean(mu1) - mean(mu2)

  cohens_d <- diff / sqrt((n*sigma1^2 + n*sigma2^2) / (n + n - 2));
  cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
}

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
