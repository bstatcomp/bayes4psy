#' @title linear_class
#' @description An S4 class for storing results of normal linear model.
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Raw data for the tested group.
#' @examples
#' summary(`linear_class`): prints summary od the fit.
#'
#' compare(`linear_class`, fit2 = `linear_class`): prints difference in slope and intercept between two groups. You can also provide the rope parameter.
#'
#' plot_difference(`linear_class`, fit2 = `linear_class`): a visualization of the difference between two groups. You can also provide the rope parameter.
#'
#' plot_samples(`linear_class`): plots density for the first group samples.
#'
#' plot_samples(`linear_class`, fit2 = `linear_class`): plots density for the first and the second group samples.
#'
#' compare_distributions(`linear_class`, fit2 = `linear_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#'
#' plot_distributions(`linear_class`): a visualization of the distribution for the first group.
#'
#' plot_distributions(`linear_class`, fit2 = `linear_class`): a visualization of the distribution for the first group and the second group.
#'
#' plot_distributions_difference(`linear_class`, fit2 = `linear_class`): a visualization of the difference between the distribution of the first group and the second group. You can also provide the rope parameter.
#'
#' plot_fit(`linear_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' traceplot(`linear_class`): traceplot for main fitted model parameters.
#'
#' @exportClass linear_class
linear_class <- setClass(
  "linear_class",
  slots = c(
    extract  = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)


#' @exportMethod summary
setMethod(f = "summary", signature(object = "linear_class"), definition = function(object) {
  # get means
  alpha <- mean(object@extract$mu_a)
  beta <- mean(object@extract$mu_b)
  sigma <- sqrt(mean(object@extract$mu_s))

  # hdi
  alpha_hdi <- mcmc_hdi(object@extract$mu_a)
  beta_hdi <- mcmc_hdi(object@extract$mu_b)
  sigma_hdi <- mcmc_hdi(object@extract$mu_s)

  # print
  cat(sprintf("intercept (alpha): %.2f, 95%% HDI: [%.2f, %.2f]\n", alpha, alpha_hdi[1], alpha_hdi[2]))
  cat(sprintf("slope (beta): %.2f, 95%% HDI: [%.2f, %.2f]\n", beta, beta_hdi[1], beta_hdi[2]))
  cat(sprintf("sigma: %.2f, 95%% HDI: [%.2f, %.2f]\n", sigma, sigma_hdi[1], sigma_hdi[2]))
})


#' @title compare
#' @description \code{compare} prints difference in intercept and slope between two groups.
#' @rdname linear_class-compare
#' @aliases compare,ANY-method
setMethod(f = "compare", signature(object = "linear_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare function are invalid, compare(linear_class, fit2 = linear_class) is required! You can also provide the rope parameters, e.g. compare(linear_class, fit2 = linear_class, rope_intercept = numeric, rope_slope = numeric)."

  if (is.null(arguments)) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept = arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope = arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  # first group data
  intercept1 <-
  slope1 <-

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
#' @description \code{plot_difference} TODO
#' @rdname linear_class-plot_difference
#' @aliases plot_difference,ANY-method
# setMethod(f = "plot_difference", signature(object = "linear_class"), definition = function(object, ...) {
#
# })


#' @title plot_samples
#' @description \code{plot_samples} plots first group samples, or the first and the second group samples.
#' @rdname linear_class-plot_samples
#' @aliases plot_samples,ANY-method
setMethod(f = "plot_samples", signature(object = "linear_class"), definition = function(object, ...) {
  # first group data
  df1 <- data.frame(intercept = object@extract$mu_a, slope = object@extract$mu_b)

  # limits
  x_min_intercept <- min(df1$intercept)
  x_max_intercept <- max(df1$intercept)
  x_min_slope <- min(df1$slope)
  x_max_slope <- max(df1$slope)

  # plot
  graph_intercept <- ggplot() +
    geom_density(data = df1, aes(x = intercept), fill = "#3182bd", alpha = 0.4, color = NA)
  graph_slope <- ggplot() +
    geom_density(data = df1, aes(x = slope), fill = "#3182bd", alpha = 0.4, color = NA)


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

      df2 <- data.frame(intercept = fit2@extract$mu_a, slope = fit2@extract$mu_b)

      # limits
      x_min_intercept <- min(x_min_intercept, df2$intercept)
      x_max_intercept <- max(x_max_intercept, df2$intercept)
      x_min_slope <- min(x_min_slope, df2$slope)
      x_max_slope <- max(x_max_slope, df2$slope)

      # plot
      graph_intercept <- graph_intercept +
        geom_density(data = df2, aes(x = intercept), fill = "#ff4e3f", alpha = 0.4, color = NA)
      graph_slope <- graph_slope +
        geom_density(data = df2, aes(x = slope), fill = "#ff4e3f", alpha = 0.4, color = NA)
    }
  }

  # limits
  diff <- x_max_intercept - x_min_intercept
  x_min_intercept <- x_min_intercept - 0.1*diff
  x_max_intercept <- x_max_intercept + 0.1*diff
  diff <- x_max_slope - x_min_slope
  x_min_slope <- x_min_slope - 0.1*diff
  x_max_slope <- x_max_slope + 0.1*diff

  # plot
  graph_intercept <- graph_intercept +
    theme_minimal() +
    xlab("intercept") +
    xlim(x_min_intercept, x_max_intercept)
  graph_slope <- graph_slope +
    theme_minimal() +
    xlab("slope") +
    xlim(x_min_slope, x_max_slope)

  graph <- plot_grid(graph_intercept, graph_slope, ncol = 2, nrow = 1, scale = 0.9)

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} TODO
#' @rdname linear_class-compare_distributions
#' @aliases compare_distributions,ANY-method
# setMethod(f = "compare_distributions", signature(object = "linear_class"), definition = function(object, ...) {
#
# })


#' @title plot_distributions
#' @description \code{plot_distributions} TODO
#' @rdname linear_class-plot_distributions
#' @aliases plot_distributions,ANY-method
# setMethod(f = "plot_distributions", signature(object = "linear_class"), definition = function(object, ...) {
#
# })


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} TODO
#' @rdname linear_class-plot_distributions_difference
#' @aliases plot_distributions_difference,ANY-method
# setMethod(f = "plot_distributions_difference", signature(object = "linear_class"), definition = function(object, ...) {
#
# })


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @rdname linear_class-plot_fit
#' @aliases plot_fit,ANY-method
setMethod(f = "plot_fit", signature(object = "linear_class"), definition = function(object) {
  df_data <- data.frame(x = object@data$x, y = object@data$y, s = object@data$s)

  n <- length(unique(df_data$s))

  x_min <- floor(min(df_data$x))
  x_max <- ceiling(max(df_data$x))

  # mean per subject
  df_data <- df_data %>% group_by(s, x) %>% summarize(y = mean(y, na.rm=TRUE))

  # fits
  df_fit <- data.frame(x=numeric, y=numeric, s=numeric)
  for (i in 1:n) {
    alpha = mean(object@extract$alpha[,i])
    beta = mean(object@extract$beta[,i])

    df <- data.frame(x = seq(x_min, x_max, 0.01),
                     y = alpha + beta*seq(x_min, x_max, 0.01),
                     s = i)

    df_fit <- rbind(df_fit, df)
  }

  # ncol
  n_col <- ceiling(sqrt(n))

  # density per subject
  graph <- ggplot() +
    geom_point(data = df_data, aes(x = x, y = y), color = "#3182bd", alpha = 0.4) +
    geom_line(data = df_fit, aes(x = x, y = y), color = "#3182bd") +
    facet_wrap(~ s, ncol = n_col) +
    theme_minimal()

  return(graph)
})


#' @title traceplot
#' @description \code{traceplot} traceplot for main fitted model parameters.
#' @rdname linear_class-traceplot
#' @aliases traceplot,ANY-method
setMethod(f = "traceplot", signature(object = "linear_class"), definition = function(object) {
  rstan::traceplot(object@fit, pars = c("mu_a", "mu_b", "mu_s"), inc_warmup = TRUE)
})
