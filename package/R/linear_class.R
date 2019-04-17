#' @title linear_class
#' @import dplyr ggplot2
#' @description An S4 class for storing results of normal linear model.
#'
#' \strong{Functions}
#'
#' summary(`linear_class`): prints summary od the fit.
#'
#' print(`linear_class`): prints a more detailed summary of the fit
#'
#' show(`linear_class`): prints a more detailed summary of the fit.
#'
#' compare(`linear_class`, fit2=`linear_class`): prints difference in slope and intercept between two groups. You can also provide the rope parameter.
#'
#' plot_difference(`linear_class`, fit2=`linear_class`): a visualization of the difference between two groups. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_samples(`linear_class`): plots density of the samples.
#'
#' plot_samples(`linear_class`, fit2=`linear_class`): plots density for the first and the second group samples.
#'
#' compare_distributions(`linear_class`, fit2=`linear_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#'
#' plot_distributions(`linear_class`): a visualization of the fitted distribution.
#'
#' plot_distributions(`linear_class`, fit2=`linear_class`): a visualization of two fitted distribution.
#'
#' plot_distributions_difference(`linear_class`, fit2=`linear_class`): a visualization of the difference between the distribution of the first group and the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_fit(`linear_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_trace(`linear_class`): traceplot for main fitted model parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Raw data for the tested group.
#' @exportClass linear_class
linear_class <- setClass(
  "linear_class",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)


#' @title summary
#' @description \code{summary} prints summary of the Bayesian linear model fit.
#' @param object linear_class object.
#' @exportMethod summary
setMethod(f="summary", signature(object="linear_class"), definition=function(object) {
  # get means
  alpha <- mean(object@extract$mu_a)
  beta <- mean(object@extract$mu_b)
  sigma <- mean(object@extract$mu_s)

  # hdi
  alpha_hdi <- mcmc_hdi(object@extract$mu_a)
  beta_hdi <- mcmc_hdi(object@extract$mu_b)
  sigma_hdi <- mcmc_hdi(object@extract$mu_s)

  # print
  cat(sprintf("intercept (alpha):\t%.2f +/- %.5f,, 95%% HDI: [%.2f, %.2f]\n",
              alpha, mcmcse::mcse(object@extract$mu_a)$se, alpha_hdi[1], alpha_hdi[2]))
  cat(sprintf("slope (beta):\t\t%.2f +/- %.5f,, 95%% HDI: [%.2f, %.2f]\n",
              beta, mcmcse::mcse(object@extract$mu_b)$se, beta_hdi[1], beta_hdi[2]))
  cat(sprintf("sigma:\t\t\t%.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
              sigma, mcmcse::mcse(object@extract$mu_s)$se, sigma_hdi[1], sigma_hdi[2]))
})


#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian linear model fit.
#' @param object linear_class object.
#' @exportMethod show
setMethod(f="show", signature(object="linear_class"), definition=function(object) {
  # print
  show(object@fit)
})


#' @title compare
#' @description \code{compare} prints difference in intercept and slope between two groups.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object, rope_intercept and rope_slope - regions of practical equivalence.
#' @rdname linear_class-compare
#' @aliases compare_linear
setMethod(f="compare", signature(object="linear_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare function are invalid, compare(linear_class, fit2=linear_class) is required! You can also provide the rope parameters, e.g. compare(linear_class, fit2=linear_class, rope_intercept=numeric, rope_slope=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept <- arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope <- arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  # first group data
  intercept1 <- object@extract$mu_a
  slope1 <- object@extract$mu_b

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    intercept2 <- fit2@extract$mu_a
    slope2 <- fit2@extract$mu_b

    cat("---------- Intercept ----------\n")
    shared_difference(y1=intercept1, y2=intercept2, rope=rope_intercept)

    cat("\n---------- Slope ----------\n")
    shared_difference(y1=slope1, y2=slope2, rope=rope_slope)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_difference
#' @description \code{plot_difference} plots difference between two groups.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object, rope_intercept and rope_slope - regions of practical equivalence, bins - number of bins in the histogram.
#' @rdname linear_class-plot_difference
#' @aliases plot_difference_linear
setMethod(f="plot_difference", signature(object="linear_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_difference function are invalid, plot_difference(linear_class, fit2=linear_class) is required! You can optionallly provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_difference(linear_class, fit2=linear_class, rope_intercept=numeric, rope_slope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept <- arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope <- arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  # first group data
  intercept1 <- object@extract$mu_a
  slope1 <- object@extract$mu_b

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    intercept2 <- fit2@extract$mu_a
    slope2 <- fit2@extract$mu_b

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph_intercept <- shared_plot_difference(y1=intercept1, y2=intercept2, rope=rope_intercept, bins=bins)
    graph_intercept <- graph_intercept +
      ggtitle("Intercept") +
      theme(plot.title=element_text(hjust=0.5))

    graph_slope <- shared_plot_difference(y1=slope1, y2=slope2, rope=rope_slope, bins=bins)
    graph_slope <- graph_slope +
      ggtitle("Slope") +
      theme(plot.title=element_text(hjust=0.5))

    graph <- cowplot::plot_grid(graph_intercept, graph_slope, ncol=2, nrow=1, scale=0.9)
    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_samples
#' @description \code{plot_samples} plots samples, or the first and the second group samples.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object.
#' @rdname linear_class-plot_samples
#' @aliases plot_samples_linear
setMethod(f="plot_samples", signature(object="linear_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  intercept <- slope <- NULL

  # first group data
  df1 <- data.frame(intercept=object@extract$mu_a, slope=object@extract$mu_b)

  # limits
  x_min_intercept <- min(df1$intercept)
  x_max_intercept <- max(df1$intercept)
  x_min_slope <- min(df1$slope)
  x_max_slope <- max(df1$slope)

  # plot
  graph_intercept <- ggplot() +
    geom_density(data=df1, aes(x=intercept), fill="#3182bd", alpha=0.4, color=NA)
  graph_slope <- ggplot() +
    geom_density(data=df1, aes(x=slope), fill="#3182bd", alpha=0.4, color=NA)

  # second group data
  df2 <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }

      df2 <- data.frame(intercept=fit2@extract$mu_a, slope=fit2@extract$mu_b)

      # limits
      x_min_intercept <- min(x_min_intercept, df2$intercept)
      x_max_intercept <- max(x_max_intercept, df2$intercept)
      x_min_slope <- min(x_min_slope, df2$slope)
      x_max_slope <- max(x_max_slope, df2$slope)

      # plot
      graph_intercept <- graph_intercept +
        geom_density(data=df2, aes(x=intercept), fill="#ff4e3f", alpha=0.4, color=NA)
      graph_slope <- graph_slope +
        geom_density(data=df2, aes(x=slope), fill="#ff4e3f", alpha=0.4, color=NA)
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
    xlab("intercept") +
    xlim(x_min_intercept, x_max_intercept)
  graph_slope <- graph_slope +
    xlab("slope") +
    xlim(x_min_slope, x_max_slope)

  graph <- cowplot::plot_grid(graph_intercept, graph_slope, ncol=2, nrow=1, scale=0.9)

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object, rope_intercept and rope_slope - regions of practical equivalence.
#' @rdname linear_class-compare_distributions
#' @aliases compare_distributions_linear
setMethod(f="compare_distributions", signature(object="linear_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(linear_class, fit2=linear_class) is required! You can also provide the rope parameter, e.g. compare_distributions(linear_class, fit2=linear_class, rope_intercept=numeric, rope_slope=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept <- arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope <- arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  n <- 100000

  # first group data
  mu_intercept1 <- mean(object@extract$mu_a)
  sigma_intercept1 <- mean(object@extract$sigma_a)
  intercept1 <- stats::rnorm(n, mean=mu_intercept1, sd=sigma_intercept1)

  mu_slope1 <- mean(object@extract$mu_b)
  sigma_slope1 <- mean(object@extract$sigma_b)
  slope1 <- stats::rnorm(n, mean=mu_slope1, sd=sigma_slope1)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    mu_intercept2 <- mean(fit2@extract$mu_a)
    sigma_intercept2 <- mean(fit2@extract$sigma_a)
    intercept2 <- stats::rnorm(n, mean=mu_intercept2, sd=sigma_intercept2)

    mu_slope2 <- mean(fit2@extract$mu_b)
    sigma_slope2 <- mean(fit2@extract$sigma_b)
    slope2 <- stats::rnorm(n, mean=mu_slope2, sd=sigma_slope2)

    cat("---------- Intercept ----------\n")
    shared_difference(y1=intercept1, y2=intercept2, rope=rope_intercept)

    cat("\n---------- Slope ----------\n")
    shared_difference(y1=slope1, y2=slope2, rope=rope_slope)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the fitted distribution, for one or two fits.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object.
#' @rdname linear_class-plot_distributions
#' @aliases plot_distributions_linear
setMethod(f="plot_distributions", signature(object="linear_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  y <- y_min <- NULL

  # first group mean data
  df_mean <- data.frame(intercept=mean(object@extract$mu_a), slope=mean(object@extract$mu_b), group="1")

  # first group samples
  n <- min(100, length(object@extract$mu_a))
  df <- data.frame(intercept=object@extract$mu_a, slope=object@extract$mu_b, group="1")
  df <- sample_n(df, n)

  # limits
  x_min <- min(object@data$x)
  x_max <- max(object@data$x)
  y_min <- min(object@data$y)
  y_max <- max(object@data$y)

  # second group data
  graph <- ggplot()
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      # second group mean data
      df_mean <- rbind(df_mean, data.frame(intercept=mean(fit2@extract$mu_a), slope=mean(fit2@extract$mu_b), group="2"))

      # second group samples
      n <- min(100, length(fit2@extract$mu_a))
      df2 <- data.frame(intercept=fit2@extract$mu_a, slope=fit2@extract$mu_b, group="2")
      df2 <- sample_n(df2, n)
      df <- rbind(df, df2)

      # limits
      x_min <- min(x_min, fit2@data$x)
      x_max <- max(x_max, fit2@data$x)
      y_min <- min(y_min, fit2@data$y)
      y_max <- max(y_max, fit2@data$y)
    }
  }

  graph <- ggplot() +
    geom_abline(data=df, aes(slope=slope, intercept=intercept, color=group), alpha=0.1, size=1) +
    geom_abline(data=df_mean, aes(slope=slope, intercept=intercept, color=group), size=1.5) +
    scale_color_manual(values=c("#3182bd", "#ff4e3f")) +
    xlim(x_min, x_max) +
    ylim(y_min, y_max) +
    xlab("") +
    ylab("") +
    theme(legend.position="none")

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} visualizes the difference between two groups.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object, rope_intercept and rope_slope - regions of practical equivalence, bins - number of bins in the histogram.
#' @rdname linear_class-plot_distributions_difference
#' @aliases plot_distributions_difference_linear
setMethod(f="plot_distributions_difference", signature(object="linear_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(linear_class, fit2=linear_class) is required! You can also provide the rope and bins (number of bins in the histogram) parameter, e.g. plot_distributions_difference(linear_class, fit2=linear_class, rope_intercept=numeric, rope_slope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept <- arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope <- arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  n <- 100000

  # first group data
  mu_intercept1 <- mean(object@extract$mu_a)
  sigma_intercept1 <- mean(object@extract$sigma_a)
  intercept1 <- stats::rnorm(n, mean=mu_intercept1, sd=sigma_intercept1)

  mu_slope1 <- mean(object@extract$mu_b)
  sigma_slope1 <- mean(object@extract$sigma_b)
  slope1 <- stats::rnorm(n, mean=mu_slope1, sd=sigma_slope1)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    mu_intercept2 <- mean(fit2@extract$mu_a)
    sigma_intercept2 <- mean(fit2@extract$sigma_a)
    intercept2 <- stats::rnorm(n, mean=mu_intercept2, sd=sigma_intercept2)

    mu_slope2 <- mean(fit2@extract$mu_b)
    sigma_slope2 <- mean(fit2@extract$sigma_b)
    slope2 <- stats::rnorm(n, mean=mu_slope2, sd=sigma_slope2)

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph_intercept <- shared_plot_difference(y1=intercept1, y2=intercept2, rope=rope_intercept, bins=bins)
    graph_intercept <- graph_intercept +
      ggtitle("Intercept") +
      theme(plot.title=element_text(hjust=0.5))

    graph_slope <- shared_plot_difference(y1=slope1, y2=slope2, rope=rope_slope, bins=bins)
    graph_slope <- graph_slope +
      ggtitle("Slope") +
      theme(plot.title=element_text(hjust=0.5))

    graph <- cowplot::plot_grid(graph_intercept, graph_slope, ncol=2, nrow=1, scale=0.9)
    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object linear_class object.
#' @rdname linear_class-plot_fit
#' @aliases plot_fit_linear
setMethod(f="plot_fit", signature(object="linear_class"), definition=function(object) {
  # init local varibales for CRAN check
  s <- x <- y <- NULL

  df_data <- data.frame(x=object@data$x, y=object@data$y, s=object@data$s)

  n <- length(unique(df_data$s))

  x_min <- floor(min(df_data$x))
  x_max <- ceiling(max(df_data$x))

  # mean per subject
  df_data <- df_data %>% group_by(s, x) %>% summarize(y=mean(y, na.rm=TRUE))

  # fits
  df_fit <- data.frame(x=numeric, y=numeric, s=numeric)
  for (i in 1:n) {
    alpha <- mean(object@extract$alpha[,i])
    beta <- mean(object@extract$beta[,i])

    df <- data.frame(x = seq(x_min, x_max, 0.01),
                     y = alpha + beta*seq(x_min, x_max, 0.01),
                     s = i)

    df_fit <- rbind(df_fit, df)
  }

  # ncol
  n_col <- ceiling(sqrt(n))

  # density per subject
  graph <- ggplot() +
    geom_point(data=df_data, aes(x=x, y=y), color="#3182bd", alpha=0.4) +
    geom_line(data=df_fit, aes(x=x, y=y), color="#3182bd") +
    facet_wrap(. ~ s, ncol=n_col)

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object linear_class object.
#' @rdname linear_class-plot_trace
#' @aliases plot_trace_linear
setMethod(f="plot_trace", signature(object="linear_class"), definition=function(object) {
  rstan::traceplot(object@fit, pars=c("mu_a", "mu_b", "mu_s"), inc_warmup=TRUE)
})
