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
#' @slot y1_samples samples for the first group.
#' @slot y2_samples Optional variable: samples for the second group.
#' @slot mu Optional variable: mean value for comparison with first group.
#' @slot sigma Optional variable: variance for comparison with first group, used in combination with `mu` parameter.
#' @slot rope Optional variable: region of practical equivalence.
#' @examples
#' ttest_results: prints difference/equality of two tested groups.
#'
#' summary(ttest_results): prints difference/equality of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#'
#' difference_plot(ttest_results): a visualization of the difference of the first group against the second group, against a mean value, or against a normal distribution with a defined mean value and variance.
#' @exportClass ttest_results
ttest_results <- setClass(
  "ttest_results",
  slots = c(
    y1_samples = "list",
    y2_samples = "list_or_null",
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


#' @rdname ttest_results-difference_plot
#' @exportMethod difference_plot
setGeneric(name = "difference_plot", function(object) standardGeneric("difference_plot"))

#' @title difference_plot
#' @description \code{difference_plot} visualizes difference/equality of two tested groups.
#' @rdname ttest_results-difference_plot
#' @aliases difference_plot,ANY-method
setMethod(f = "difference_plot", signature(object = "ttest_results"), definition = function(object) {
  # draw from samples for first group
  n <- 100000
  y1 <- rt.scaled(n, df = mean(object@y1_samples$nu), mean = mean(object@y1_samples$mu), sd = mean(object@y1_samples$sigma))

  # generate data for second group
  y2 <- NULL
  if (!is.null(object@y2_samples))
    y2 <- rt.scaled(n, df = mean(object@y2_samples$nu), mean = mean(object@y2_samples$mu), sd = mean(object@y2_samples$sigma))
  else if (!is.null(object@mu)) {
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
  if (!is.null(object@rope)) {
    graph <- graph +
      geom_segment(aes(x = -object@rope, xend = object@rope, y = y_max * 0.01, yend = y_max * 0.01), size = 3, color = "grey50")
  }

  # style and labels
  graph <- graph +
    theme_minimal() +
    labs(title = "Difference plot", x = "Value", y = "") +
    theme(plot.title = element_text(hjust = 0.5))

  graph
})


#' @rdname ttest_results-comparison_plot
#' @exportMethod comparison_plot
setGeneric(name = "comparison_plot", function(object) standardGeneric("comparison_plot"))

#' @title comparison_plot
#' @description \code{comparison_plot} visualizes both tested groups.
#' @rdname ttest_results-comparison_plot
#' @aliases comparison_plot,ANY-method
setMethod(f = "comparison_plot", signature(object = "ttest_results"), definition = function(object) {
  # draw from samples for first group
  n <- 10000
  y1_nu <- mean(object@y1_samples$nu)
  y1_mu <- mean(object@y1_samples$mu)
  y1_sigma <- mean(object@y1_samples$sigma)

  # get x range
  x_min <- y1_mu - 4 * y1_sigma
  x_max <- y1_mu + 4 * y1_sigma

  y2_plot <- NULL
  # is second group calculated by stan fit?
  if (!is.null(object@y2_samples)) {
    y2_nu <- mean(object@y1_samples$nu)
    y2_mu <- mean(object@y2_samples$mu)
    y2_sigma <- mean(object@y2_samples$sigma)

    x_min <- min(x_min, y2_mu - 4 * y2_sigma)
    x_max <- max(x_max, y2_mu + 4 * y2_sigma)

    y2_plot <- stat_function(fun = dt.scaled, n = n, args = list(df = y2_nu, mean = y2_mu, sd = y2_sigma), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
  }
  # second group is given as a normal distribution
  else if (!is.null(object@mu) && !is.null(object@sigma)) {
    y2_mu <- object@mu
    y2_sigma <- object@sigma

    x_min <- min(x_min, y2_mu - 4 * y2_sigma)
    x_max <- max(x_max, y2_mu + 4 * y2_sigma)

    y2_plot <- stat_function(fun = dnorm, n = n, args = list(mean = y2_mu, sd = y2_sigma), geom = 'area', fill = '#ff4e3f', alpha = 0.4)
  }
  # second group is just a constant number, part 1 (because we need max y for vertical line)
  else if (!is.null(object@mu)) {
    y2_mu <- object@mu

    x_min <- min(x_min, y2_mu - (y2_mu * 0.05))
    x_max <- max(x_max, y2_mu + (y2_mu * 0.05))
  }

  # plot
  x_range <- data.frame(value = c(x_min, x_max))

  graph <- ggplot(data = x_range, aes(x = value)) +
    stat_function(fun = dt.scaled, n = n, args = list(df = y1_nu, mean = y1_mu, sd = y1_sigma), geom = 'area', fill = '#3182bd', alpha = 0.4) +
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

  graph
})


### Helper functions
# print difference between the first group and the second object (a group, a mean value or a normal distribution)
difference_print <- function(object) {
  # draw from samples for first group
  n <- 100000
  y1 <- rt.scaled(n, df = mean(object@y1_samples$nu), mean = mean(object@y1_samples$mu), sd = mean(object@y1_samples$sigma))

  # generate data for second group
  y2 <- NULL
  if (!is.null(object@y2_samples))
    y2 <- rt.scaled(n, df = mean(object@y2_samples$nu), mean = mean(object@y2_samples$mu), sd = mean(object@y2_samples$sigma))
  else if (!is.null(object@mu)) {
    if (is.null(object@sigma))
      object@sigma = 0;

    y2 <- rnorm(n, mean(object@mu), mean(object@sigma))
  }

  # 1 > 2
  y1_greater <- sum((y1 - y2) > object@rope) / n
  cat("Probabilities:\n  - Group 1 > Group 2: ", y1_greater)

  # 2 > 1
  y2_greater <- sum((y2 - y1) > object@rope) / n
  cat("\n  - Group 2 > Group 1: ", y2_greater)

  # equal
  if (is.null(object@rope)) {
    cat("\n  - Equal: NA")
  } else {
    equal <- sum(abs(y1 - y2) <= object@rope) / n
    cat("\n  - Equal: ", equal, "\n")
  }
}
