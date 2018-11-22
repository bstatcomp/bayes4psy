#' An S4 class for storing results of reaction time Bayesian model.
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
#' @examples
#' summary(`reaction_time_class`): prints summary od the fit.
#'
#' compare(`reaction_time_class`, fit2 = `reaction_time_class`): prints difference in reaction times between two groups. You can also provide the rope parameter.
#'
#' plot_difference(`reaction_time_class`, fit2 = `reaction_time_class`): a visualization of the difference between two groups. You can also provide the rope parameter.
#'
#' plot_comparison(`reaction_time_class`, fit2 = `reaction_time_class`): plots density for the first and the second group.
#'
#' compare_distributions(`reaction_time_class`, fit2 = `reaction_time_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter.
#'
#' plot_distributions(`reaction_time_class`, fit2 = `reaction_time_class`): a visualization of the distribution for the first group and the second group.
#'
#' plot_distributions_difference(`reaction_time_class`, fit2 = `reaction_time_class`): a visualization of the difference between the distribution of the first group and the second group. You can also provide the rope parameter.
#'
#' plot_fit(`reaction_time_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' traceplot(`reaction_time_class`): traceplot for main fitted model parameters.
#'
#' @exportClass reaction_time_class
reaction_time_class <- setClass(
  "reaction_time_class",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)

#' @exportMethod summary
setMethod(f = "summary", signature(object = "reaction_time_class"), definition = function(object) {
  # get means
  mu <- mean(object@extract$mu_m)
  sigma <- mean(object@extract$mu_s)
  lambda <- mean(object@extract$mu_l)

  # print
  cat(sprintf("mu: %.2f\n", mu))
  cat(sprintf("sigma: %.2f\n", sigma))
  cat(sprintf("lambda: %.2f\n", lambda))
})


#' @title compare
#' @description \code{compare} prints difference in reaction times between two groups.
#' @rdname reaction_time_class-compare
#' @aliases compare,ANY-method
setMethod(f = "compare", signature(object = "reaction_time_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare function are invalid, compare(reaction_time_class, fit2 = reaction_time_class) is required! You can also provide the rope parameter, e.g. compare(reaction_time_class, fit2 = reaction_time_class, rope = numeric)."

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
  par <- NULL
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (par != "mu" || par != "lambda") {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are mu and lambda! Using the default setting for comparison.", par)
      warning(w)
      par <- NULL
    }
  }

  if (is.null(par)) {
    y1 <- object@extract$mu_m + 1/object@extract$mu_l
  } else if (par == "mu") {
    y1 <- object@extract$mu_m
  } else if (par == "lambda") {
    y1 <- object@extract$mu_l
  }

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }

    if (is.null(par)) {
      y2 <- fit2@extract$mu_m + 1/fit2@extract$mu_l
    } else if (par == "mu") {
      y2 <- fit2@extract$mu_m
    } else if (par == "lambda") {
      y2 <- fit2@extract$mu_l
    }

    shared_difference(y1 = y1, y2 = y2, rope = rope)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_difference
#' @description \code{plot_difference} a visualization of the difference between two groups.
#' @rdname reaction_time_class-plot_difference
#' @aliases plot_difference,ANY-method
setMethod(f = "plot_difference", signature(object = "reaction_time_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_difference function are invalid, plot_difference(reaction_time_class, fit2 = reaction_time_class) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_difference(reaction_time_class, fit2 = reaction_time_class, rope = numeric, bins = numeric)."

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
  y1 <- object@extract$mu_m + 1/object@extract$mu_l

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y2 <- fit2@extract$mu_m + 1/fit2@extract$mu_l

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    shared_plot_difference(y1 = y1, y2 = y2, rope = rope, bins = bins)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_comparison
#' @description \code{plot_comparison} plots density for the first and the second group.
#' @rdname reaction_time_class-plot_comparison
#' @aliases plot_comparison,ANY-method
setMethod(f = "plot_comparison", signature(object = "reaction_time_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_comparison function are invalid, plot_comparison(reaction_time_class, fit2 = reaction_time_class) is required!"

  if (is.null(arguments)) {
    warning(wrong_arguments)
    return()
  }

  # first group data
  df1 <- data.frame(value = object@extract$mu_m + 1/object@extract$mu_l)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    df2 <- data.frame(value = fit2@extract$mu_m + 1/fit2@extract$mu_l)

    # limits
    x_min <- min(df1$value, df2$value)
    x_max <- max(df1$value, df2$value)

    diff <- x_max - x_min

    x_min <- x_min - 0.1*diff
    x_max <- x_max + 0.1*diff

    # plot
    graph <- ggplot() +
      geom_density(data = df1, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
      geom_density(data = df2, aes(x = value), fill = "#ff4e3f", alpha = 0.4, color = NA) +
      theme_minimal() +
      xlab("value") +
      xlim(x_min, x_max)

    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#' @rdname reaction_time_class-compare_distributions
#' @aliases compare_distributions,ANY-method
setMethod(f = "compare_distributions", signature(object = "reaction_time_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(reaction_time_class, fit2 = reaction_time_class) is required! You can also provide the rope parameter, e.g. compare_distributions(reaction_time_class, fit2 = reaction_time_class, rope = numeric)."

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

  n <- 100000

  # first group data
  mu_m1 <- mean(object@extract$mu_m)
  mu_s1 <- mean(object@extract$mu_s)
  mu_l1 <- mean(object@extract$mu_l)
  y1 <- remg(n, mu = mu_m1, sigma = mu_s1, lambda = mu_l1)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    mu_m2 <- mean(fit2@extract$mu_m)
    mu_s2 <- mean(fit2@extract$mu_s)
    mu_l2 <- mean(fit2@extract$mu_l)
    y2 <- remg(n, mu = mu_m2, sigma = mu_s2, lambda = mu_l2)

    shared_difference(y1 = y1, y2 = y2, rope = rope)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the distribution for the first group and the second group.
#' @rdname reaction_time_class-plot_distributions
#' @aliases plot_distributions,ANY-method
setMethod(f = "plot_distributions", signature(object = "reaction_time_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions function are invalid, plot_distributions(reaction_time_class, fit2 = reaction_time_class) is required!"

  if (is.null(arguments)) {
    warning(wrong_arguments)
    return()
  }

  n <- 100000

  # first group data
  mu_m1 <- mean(object@extract$mu_m)
  mu_s1 <- mean(object@extract$mu_s)
  mu_l1 <- mean(object@extract$mu_l)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    mu_m2 <- mean(fit2@extract$mu_m)
    mu_s2 <- mean(fit2@extract$mu_s)
    mu_l2 <- mean(fit2@extract$mu_l)

    x_min <- min(mu_m1 - 4*mu_s1, mu_m2 - 4*mu_s2)
    x_max <- max(mu_m1 + 1/mu_l1 + 4*mu_s1, mu_m2 + 1/mu_l2 + 4*mu_s2)

    x_max <- ceiling(max(object@data$rt, fit2@data$rt))
    df_x <- data.frame(value = c(0, x_max))

    # plot
    graph <- ggplot(data = df_x, aes(x = value)) +
      stat_function(fun = demg, n = n, args = list(mu = mu_m1, sigma = mu_s1, lambda = mu_l1), geom = 'area', fill = '#3182bd', alpha = 0.4) +
      stat_function(fun = demg, n = n, args = list(mu = mu_m2, sigma = mu_s2, lambda = mu_l2), geom = 'area', fill = '#ff4e3f', alpha = 0.4) +
      theme_minimal() +
      xlab("value")

    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the second group.
#' @rdname reaction_time_class-plot_distributions_difference
#' @aliases plot_distributions_difference,ANY-method
setMethod(f = "plot_distributions_difference", signature(object = "reaction_time_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(reaction_time_class, fit2 = reaction_time_class) is required! You can also provide the rope and bins (number of bins in the histogram) parameter, e.g. plot_distributions_difference(reaction_time_class, fit2 = reaction_time_class, rope = numeric, bins = numeric)."

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

  n <- 100000

  # first group data
  mu_m1 <- mean(object@extract$mu_m)
  mu_s1 <- mean(object@extract$mu_s)
  mu_l1 <- mean(object@extract$mu_l)
  y1 <- remg(n, mu = mu_m1, sigma = mu_s1, lambda = mu_l1)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    mu_m2 <- mean(fit2@extract$mu_m)
    mu_s2 <- mean(fit2@extract$mu_s)
    mu_l2 <- mean(fit2@extract$mu_l)
    y2 <- remg(n, mu = mu_m2, sigma = mu_s2, lambda = mu_l2)

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    shared_plot_difference(y1 = y1, y2 = y2, rope = rope, bins = bins)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @rdname reaction_time_class-plot_fit
#' @aliases plot_fit,ANY-method
setMethod(f = "plot_fit", signature(object = "reaction_time_class"), definition = function(object) {
  df_data <- data.frame(rt = object@data$rt, s = object@data$s)

  df_fit <- NULL
  n <- length(unique(object@data$s))

  x_min <- floor(min(object@data$rt))
  x_max <- ceiling(max(object@data$rt))

  for (i in 1:n) {
    df <- data.frame(x = seq(x_min, x_max, 0.01),
                     s = i,
                     y = demg(seq(0, x_max, 0.01),
                              mu = mean(object@extract$mu[,i]),
                              sigma = mean(object@extract$sigma[,i]),
                              lambda = mean(object@extract$lambda[,i])))

    df_fit <- rbind(df_fit, df)
  }

  # ncol
  n_col <- ceiling(sqrt(n))

  # density per subject
  graph <- ggplot(df_data, aes(x = rt)) +
    geom_density(fill = "#3182bd", alpha = 0.4, color = NA) +
    geom_line(data = df_fit, aes(x = x, y = y)) +
    facet_wrap(~ s, ncol = n_col) +
    xlab("reaction time") +
    theme_minimal()

  return(graph)
})


#' @title traceplot
#' @description \code{traceplot} traceplot for main fitted model parameters.
#' @rdname reaction_time_class-traceplot
#' @aliases traceplot,ANY-method
setMethod(f = "traceplot", signature(object = "reaction_time_class"), definition = function(object) {
  rstan::traceplot(object@fit, pars = c("mu_m", "mu_s", "mu_l"), inc_warmup = TRUE)
})
