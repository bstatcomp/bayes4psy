#' An S4 class for storing results of reaction time Bayesian model.
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
#' @examples
#' summary(`reaction_times_results`): prints summary od the fit.
#'
#' compare(`reaction_times_results`, `reaction_times_results`): prints difference in reaction times between two groups.
#'
#' plot_difference(`reaction_times_results`, `reaction_times_results`): a visualization of the difference between two groups.
#'
#' plot_comparison(`reaction_times_results`, `reaction_times_results`): plots density for the first and the second group.
#'
#' compare_distributions(`reaction_times_results`, `reaction_times_results`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#'
#' plot_distributions(`reaction_times_results`, `reaction_times_results`): a visualization of the distribution for the first group and the second group.
#'
#' plot_distributions_difference(`reaction_times_results`, `reaction_times_results`): a visualization of the difference between the distribution of the first group and the second group.
#'
#' plot_fit(`reaction_times_results`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' traceplot(`reaction_times_results`): traceplot for main fitted model parameters.
#'
#' @exportClass reaction_times_results
reaction_times_results <- setClass(
  "reaction_times_results",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)

#' @exportMethod summary
setMethod(f = "summary", signature(object = "reaction_times_results"), definition = function(object) {
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
#' @rdname reaction_times_results-compare
#' @aliases compare,ANY-method
setMethod(f = "compare", signature(object = "reaction_times_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "reaction_times_results") {
    # first group data
    y1 <- object@extract$mu_m + 1/object@extract$mu_l

    # second group data
    object2 <- list(...)[[1]]
    y2 <- object2@extract$mu_m + 1/object2@extract$mu_l

    shared_difference(y1, y2)
  } else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(reaction_times_results, reaction_times_results) is required!")
  }
})


#' @title plot_difference
#' @description \code{plot_difference} a visualization of the difference between two groups.
#' @rdname reaction_times_results-plot_difference
#' @aliases plot_difference,ANY-method
setMethod(f = "plot_difference", signature(object = "reaction_times_results"), definition = function(object, ..., bins = 30) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "reaction_times_results") {
    # first group data
    y1 <- object@extract$mu_m + 1/object@extract$mu_l

    # second group data
    object2 <- list(...)[[1]]
    y2 <- object2@extract$mu_m + 1/object2@extract$mu_l

    # call plot difference shared function from shared plots
    shared_plot_difference(y1, y2)
  } else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(reaction_times_results, reaction_times_results) is required!")
  }
})


#' @title plot_comparison
#' @description \code{plot_comparison} plots density for the first and the second group.
#' @rdname reaction_times_results-plot_comparison
#' @aliases plot_comparison,ANY-method
setMethod(f = "plot_comparison", signature(object = "reaction_times_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "reaction_times_results") {
    # first group data
    df1 <- data.frame(value = object@extract$mu_m + 1/object@extract$mu_l)

    # second group data
    object2 <- list(...)[[1]]
    df2 <- data.frame(value = object2@extract$mu_m + 1/object2@extract$mu_l)

    # limits
    x_min <- min(df1$value, df2$value)
    x_max <- max(df1$value, df2$value)

    diff <- x_max - x_min

    x_min <- x_min - (0.1 * diff)
    x_max <- x_max + (0.1 * diff)

    # plot
    graph <- ggplot() +
      geom_density(data = df1, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
      geom_density(data = df2, aes(x = value), fill = "#ff4e3f", alpha = 0.4, color = NA) +
      theme_minimal() +
      xlab("Value") +
      xlim(x_min, x_max)

    return(graph)
  } else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(reaction_times_results, reaction_times_results) is required!")
  }
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#' @rdname reaction_times_results-compare_distributions
#' @aliases compare_distributions,ANY-method
setMethod(f = "compare_distributions", signature(object = "reaction_times_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "reaction_times_results") {
    n <- 100000

    # first group data
    mu_m1 <- object@extract$mu_m
    mu_s1 <- object@extract$mu_s
    mu_l1 <- object@extract$mu_l
    y1 <- remg(n, mu = mu_m1, sigma = mu_s1, lambda = mu_l1)

    # second group data
    object2 <- list(...)[[1]]
    mu_m2 <- object2@extract$mu_m
    mu_s2 <- object2@extract$mu_s
    mu_l2 <- object2@extract$mu_l
    y2 <- remg(n, mu = mu_m2, sigma = mu_s2, lambda = mu_l2)

    shared_difference(y1, y2)
  } else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(reaction_times_results, reaction_times_results) is required!")
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the distribution for the first group and the second group.
#' @rdname reaction_times_results-plot_distributions
#' @aliases plot_distributions,ANY-method
setMethod(f = "plot_distributions", signature(object = "reaction_times_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "reaction_times_results") {
    object2 <- list(...)[[1]]

    # first group data
    mu_m1 <- mean(object@extract$mu_m)
    mu_s1 <- mean(object@extract$mu_s)
    mu_l1 <- mean(object@extract$mu_l)

    # second group data
    object2 <- list(...)[[1]]
    mu_m2 <- mean(object2@extract$mu_m)
    mu_s2 <- mean(object2@extract$mu_s)
    mu_l2 <- mean(object2@extract$mu_l)

    x_min <- min(mu_m1 - 4 * mu_s1, mu_m2 - 4 * mu_s2)
    x_max <- max(mu_m1 + 1/mu_l1 + 4 * mu_s1, mu_m2 + 1/mu_l2 + 4 * mu_s2)

    x_max <- ceiling(max(object@data$rt, object2@data$rt))
    df_x <- data.frame(value = c(0, x_max))

    # plot
    graph <- ggplot(data = df_x, aes(x = value)) +
      stat_function(fun = demg, n = n, args = list(mu = mu_m1, sigma = mu_s1, lambda = mu_l1), geom = 'area', fill = '#3182bd', alpha = 0.4) +
      stat_function(fun = demg, n = n, args = list(mu = mu_m2, sigma = mu_s2, lambda = mu_l2), geom = 'area', fill = '#ff4e3f', alpha = 0.4) +
      theme_minimal() +
      xlab("Value")

    return(graph)
  } else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(reaction_times_results, reaction_times_results) is required!")
  }
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the second group.
#' @rdname reaction_times_results-plot_distributions_difference
#' @aliases plot_distributions_difference,ANY-method
setMethod(f = "plot_distributions_difference", signature(object = "reaction_times_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "reaction_times_results") {
    n <- 100000

    # first group data
    mu_m1 <- object@extract$mu_m
    mu_s1 <- object@extract$mu_s
    mu_l1 <- object@extract$mu_l
    y1 <- remg(n, mu = mu_m1, sigma = mu_s1, lambda = mu_l1)

    # second group data
    object2 <- list(...)[[1]]
    mu_m2 <- object2@extract$mu_m
    mu_s2 <- object2@extract$mu_s
    mu_l2 <- object2@extract$mu_l
    y2 <- remg(n, mu = mu_m2, sigma = mu_s2, lambda = mu_l2)

    shared_plot_difference(y1, y2)
  } else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(reaction_times_results, reaction_times_results) is required!")
  }
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @rdname reaction_times_results-plot_fit
#' @aliases plot_fit,ANY-method
setMethod(f = "plot_fit", signature(object = "reaction_times_results"), definition = function(object) {
  df_data <- data.frame(rt = object@data$rt, s = object@data$s)

  df_fit <- NULL
  n <- length(unique(object@data$s))

  x_max <- ceiling(max(object@data$rt))

  for (i in 1:n) {
    df <- data.frame(x = seq(0, x_max, 0.01),
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
    xlab("Reaction time") +
    theme_minimal()

  return(graph)
})


#' @title traceplot
#' @description \code{traceplot} traceplot for main fitted model parameters.
#' @rdname reaction_times_results-traceplot
#' @aliases traceplot,ANY-method
setMethod(f = "traceplot", signature(object = "reaction_times_results"), definition = function(object) {
  rstan::traceplot(object@fit, pars = c("mu_m", "mu_s", "mu_l"), inc_warmup = TRUE)
})
