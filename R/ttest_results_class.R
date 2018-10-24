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
#' @slot extract Extractf from Stan fit.
#' @slot mu Optional variable: mean value for comparison with first group.
#' @slot sigma Optional variable: standard deviation for comparison with first group, used in combination with `mu` parameter.
#' @slot rope Optional variable: rope interval used in comparisons.
#' @examples
#' ttest_results: prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#'
#' summary(ttest_results): prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#'
#' compare_groups(ttest_results): prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#'
#' plot_difference(ttest_results): a visualization of the difference of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#'
#' plot_comparison(ttest_results): plots histogram for the first group and the histogram of the second group, or a mean value in case second group is defined as a normal distribution or as a constant.
#'
#' compare_distributions(ttest_results): draws samples from distribution of the first group and compares them against samples from the second group, against a mean value, or against samples from a normal distribution with a defined mean value and variance.
#'
#' plot_distributions(ttest_results): a visualization of the distribution for the first group and the distribution or a constant value for the second group.
#'
#' plot_distributions_difference(ttest_results): a visualization of the difference between the distribution of the first group and the distribution or a constant value for the second group.
#'
#' @exportClass ttest_results
ttest_results <- setClass(
  "ttest_results",
  slots = c(
    extract  = "list",
    mu = "numeric_or_null",
    sigma = "numeric_or_null",
    rope = "numeric_or_null"
  ),

  validity = ttest_results_check
)


#' @exportMethod show
setMethod(f = "show", definition = function(object) {
  difference_print(object)
})


#' @exportMethod summary
setMethod(f = "summary", signature(object = "ttest_results"), definition = function(object) {
  difference_print(object)
})


#' @rdname ttest_results-plot_difference
#' @exportMethod plot_difference
setGeneric(name = "plot_difference", function(object) standardGeneric("plot_difference"))

#' @title plot_difference
#' @description \code{plot_difference} visualizes difference/equality of two tested groups.
#' @rdname ttest_results-plot_difference
#' @aliases plot_difference,ANY-method
setMethod(f = "plot_difference", signature(object = "ttest_results"), definition = function(object) {
  # prepare rope
  rope <- prepare_rope(object@rope)

  # draw from samples for first group
  mu1 <- object@extract$mu[, 1]

  # generate data for second group
  mu2 <- NULL
  if (ncol(object@extract$mu) == 2) {
    mu2 <- object@extract$mu[, 2]
  } else if (!is.null(object@mu)) {
    mu2 <- object@mu
  }

  # difference
  diff <- data.frame(value = mu1 - mu2)

  # get 95% hdi
  hdi <- mcmc_hdi(diff$value)

  # mean difference
  mean_diff <- mean(diff$value)

  # get x range
  x_min <- min(diff)
  x_max <- max(diff)
  if (!(rope[1] == 0 && rope[2] == 0)) {
    x_min <- min(x_min, rope[1])
    x_max <- max(x_max, rope[2])
  }

  # basic histogram chart
  graph <- ggplot() +
    geom_histogram(data = diff, aes(x = value), fill = "#3182bd", alpha = 0.5, bins = 30) +
    xlim(x_min, x_max)

  # add mean
  y_max <- max(ggplot_build(graph)$data[[1]]$count)
  graph <- graph +
    geom_segment(aes(x = mean_diff, xend = mean_diff, y = 0, yend = y_max * 1.05), size = 1.5, color = "#3182bd") +
    geom_text(aes(label = sprintf("%.2f", mean_diff), x = mean_diff, y = y_max * 1.08), size = 4)

  # add CI
  graph <- graph +
    geom_segment(aes(x = hdi[1], xend = hdi[2], y = -(y_max * 0.01), yend = -(y_max * 0.01)), size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f", hdi[1]), x = hdi[1], y = -(y_max * 0.04)), size = 4) +
    geom_text(aes(label = sprintf("%.2f", hdi[2]), x = hdi[2], y = -(y_max * 0.04)), size = 4)

  # add ROPE interval?
  if (!(rope[1] == 0 && rope[2] == 0)) {
    graph <- graph +
      geom_segment(aes(x = rope[1], xend = rope[2], y = y_max * 0.01, yend = y_max * 0.01), size = 3, color = "grey50")
  }

  # style and labels
  graph <- graph +
    theme_minimal() +
    labs(title = "Difference plot", x = "Value", y = "") +
    theme(plot.title = element_text(hjust = 0.5))

  suppressWarnings(print(graph))
})


#' @rdname ttest_results-plot_comparison
#' @exportMethod plot_comparison
setGeneric(name = "plot_comparison", function(object) standardGeneric("plot_comparison"))

#' @title plot_comparison
#' @description \code{plot_comparison} visualizes both tested groups.
#' @rdname ttest_results-plot_comparison
#' @aliases plot_comparison,ANY-method
setMethod(f = "plot_comparison", signature(object = "ttest_results"), definition = function(object) {
  # draw from samples for first group
  mu1 <- object@extract$mu[, 1]
  df1 <- data.frame(value = mu1)

  # get x range
  mu1_sd <- sd(mu1)
  mu1_mean <- mean(mu1)
  x_min <- mu1_mean - 4 * mu1_sd
  x_max <- mu1_mean + 4 * mu1_sd

  mu2 <- NULL
  # is second group calculated by stan fit?
  if (ncol(object@extract$mu) == 2) {
    mu2 <- object@extract$mu[, 2]

    mu2_sd <- sd(mu2)
    mu2_mean <- mean(mu2)
    x_min <- min(x_min, mu2_mean - 4 * mu2_sd)
    x_max <- max(x_max, mu2_mean + 4 * mu2_sd)

    df2 <- data.frame(value = mu2)
  # second group is given as a normal distribution or a constant
  } else if (!is.null(object@mu)) {
    mu2 <- object@mu

    x_min <- min(x_min, mu2)
    x_max <- max(x_max, mu2)
  }

  # plot
  graph <- ggplot() +
    geom_histogram(data = df1, aes(x = value), fill = "#3182bd", alpha = 0.5, bins = 30) +
    theme_minimal() +
    labs(title = "Comparison plot", x = "Value", y = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(x_min, x_max)


  if (is.null(object@mu)) {
    graph <- graph +
      geom_histogram(data = df2, aes(x = value), fill = "#ff4e3f", alpha = 0.5, bins = 30)
  } else {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = mu2, xend = mu2, y = 0, yend = y_max[2] * 1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", mu2), x = mu2, y = y_max[2] * 1.08), size = 4)
  }

  suppressWarnings(print(graph))
})


#' @rdname ttest_results-compare_distributions
#' @exportMethod compare_distributions
setGeneric(name = "compare_distributions", function(object) standardGeneric("compare_distributions"))

#' @title compare_distributions
#' @description \code{compare_distributions} prints difference/equality of samples drawn from fitted distributions.
#' @rdname ttest_results-compare_distributions
#' @aliases compare_distributions,ANY-method
setMethod(f = "compare_distributions", signature(object = "ttest_results"), definition = function(object) {
  # prepare rope
  rope <- prepare_rope(object@rope)

  # draw from samples for first group
  n <- 100000
  nu <- mean(object@extract$nu)
  y1 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu[, 1]), sd = mean(object@extract$sigma[, 1]))

  # generate data for second group
  y2 <- NULL
  if (ncol(object@extract$mu) == 2) {
    y2 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu[, 2]), sd = mean(object@extract$sigma[, 2]))
  } else if (!is.null(object@mu)) {
    if (is.null(object@sigma))
      object@sigma = 0;

    y2 <- rnorm(n, object@mu, object@sigma)
  }

  y_diff <- y1 - y2

  # 1 > 2
  y1_smaller <- sum(y_diff < rope[1]) / n
  cat("Probabilities:\n  - Group 1 < Group 2: ", y1_smaller)

  # 2 > 1
  y1_greater <- sum(y_diff > rope[2]) / n
  cat("\n  - Group 1 > Group 2: ", y1_greater)

  # equal
  if (rope[1] == 0 && rope[2] == 0) {
    cat("\n  - Equal: NA\n")
  } else {
    equal <- sum((y_diff > rope[1]) & (y_diff < rope[2])) / n
    cat("\n  - Equal: ", equal, "\n")
  }
})


#' @rdname ttest_results-plot_distributions_difference
#' @exportMethod plot_distributions_difference
setGeneric(name = "plot_distributions_difference", function(object) standardGeneric("plot_distributions_difference"))

#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} visualizes difference/equality of distributions underlying the data.
#' @rdname ttest_results-plot_distributions_difference
#' @aliases plot_distributions_difference,ANY-method
setMethod(f = "plot_distributions_difference", signature(object = "ttest_results"), definition = function(object) {
  # prepare rope
  rope <- prepare_rope(object@rope)

  # draw from samples for first group
  n <- 100000
  nu <- mean(object@extract$nu)
  y1 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu[, 1]), sd = mean(object@extract$sigma[, 1]))

  # generate data for second group
  y2 <- NULL
  if (ncol(object@extract$mu) == 2) {
    y2 <- rt.scaled(n, df = nu, mean = mean(object@extract$mu[, 2]), sd = mean(object@extract$sigma[, 2]))
  } else if (!is.null(object@mu)) {
    if (is.null(object@sigma))
      object@sigma = 0;

    y2 <- rnorm(n, mean(object@mu), mean(object@sigma))
  }

  # difference
  diff <- data.frame(value = y1 - y2)

  # get 95% hdi
  hdi <- mcmc_hdi(diff$value)

  # mean difference
  mean_diff <- mean(diff$value)

  # basic histogram chart
  graph <- ggplot() +
    geom_histogram(data = diff, aes(x = value), fill = "#3182bd", alpha = 0.5, bins = 30)

  # add mean
  y_max <- max(ggplot_build(graph)$data[[1]]$count)
  graph <- graph +
    geom_segment(aes(x = mean_diff, xend = mean_diff, y = 0, yend = y_max * 1.05), size = 1.5, color = "#3182bd") +
    geom_text(aes(label = sprintf("%.2f", mean_diff), x = mean_diff, y = y_max * 1.08), size = 4)

  # add CI
  graph <- graph +
    geom_segment(aes(x = hdi[1], xend = hdi[2], y = -(y_max * 0.01), yend = -(y_max * 0.01)), size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f", hdi[1]), x = hdi[1], y = -(y_max * 0.04)), size = 4) +
    geom_text(aes(label = sprintf("%.2f", hdi[2]), x = hdi[2], y = -(y_max * 0.04)), size = 4)

  # add ROPE interval?
  if (!(rope[1] == 0 && rope[2] == 0)) {
    graph <- graph +
      geom_segment(aes(x = rope[1], xend = rope[2], y = y_max * 0.01, yend = y_max * 0.01), size = 3, color = "grey50")
  }

  # style and labels
  graph <- graph +
    theme_minimal() +
    labs(title = "Distributions difference plot", x = "Value", y = "") +
    theme(plot.title = element_text(hjust = 0.5))

  return(graph)
})


#' @rdname ttest_results-plot_distributions
#' @exportMethod plot_distributions
setGeneric(name = "plot_distributions", function(object) standardGeneric("plot_distributions"))

#' @title plot_distributions
#' @description \code{plot_distributions} visualizes distributions underlying tested groups.
#' @rdname ttest_results-plot_distributions
#' @aliases plot_distributions,ANY-method
setMethod(f = "plot_distributions", signature(object = "ttest_results"), definition = function(object) {
  # draw from samples for first group
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
  x_range <- data.frame(value = c(x_min, x_max))

  graph <- ggplot(data = x_range, aes(x = value)) +
    stat_function(fun = dt.scaled, n = n, args = list(df = nu, mean = y1_mu, sd = y1_sigma), geom = 'area', fill = '#3182bd', alpha = 0.4) +
    y2_plot +
    theme_minimal() +
    labs(title = "Comparison plot", x = "Value", y = "") +
    theme(plot.title = element_text(hjust = 0.5))

  if (!is.null(object@mu) && is.null(object@sigma)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = y2_mu, xend = y2_mu, y = 0, yend = y_max[2] * 1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", y2_mu), x = y2_mu, y = y_max[2] * 1.08), size = 4)
  }

  return(graph)
})


### Helper functions
# print difference between the first group and the second object (a group, a mean value or a normal distribution)
difference_print <- function(object) {
  # prepare rope
  rope <- prepare_rope(object@rope)

  # samples for first group
  mu1 <- object@extract$mu[, 1]
  n <- length(mu1)

  # generate data for second group
  mu2 <- NULL
  if (ncol(object@extract$mu) == 2) {
    mu2 <- object@extract$mu[, 2]
  } else if (!is.null(object@mu)) {
    mu2 <- rep(object@mu, n)
  }

  mu_diff <- mu1 - mu2

  # 1 > 2
  mu1_smaller <- sum(mu_diff < rope[1]) / n
  cat("Probabilities:\n  - Group 1 < Group 2: ", mu1_smaller)

  # 2 > 1
  mu1_greater <- sum(mu_diff > rope[2]) / n
  cat("\n  - Group 1 > Group 2: ", mu1_greater)

  # equal
  if (rope[1] == 0 && rope[2] == 0) {
    cat("\n  - Equal: NA\n")
  } else {
    equal <- sum((mu_diff > rope[1]) & (mu_diff < rope[2])) / n
    cat("\n  - Equal: ", equal, "\n")
  }
}

# prepare rope
prepare_rope <- function(rope) {
  # rope is NULL
  if (is.null(rope)) {
    rope[1] = 0
    rope[2] = 0
    return(rope)
  }

  # validity check for rope
  if (length(rope) > 2) {
    warning("You provided more than two values for the ROPE interval! Rope value was thus set to 0.")
    rope[1] = 0
    rope[2] = 0
    return(rope)
  }
  else if (!is.null(rope) && length(rope) == 1 && rope < 0) {
    warning("When a single number is provided for the ROPE interval it should be positive or 0! Rope value was thus set to 0.")
    rope[1] = 0
    rope[2] = 0
    return(rope)
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
