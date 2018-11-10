#' An S4 class for storing results of successes (true/false) Bayesian model.
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
#' @examples
#' summary(`successes_results`): prints summary od the fit.
#'
#' compare(`successes_results`, `successes_results`): prints difference in successfulness of two groups.
#'
#' plot_difference(`successes_results`, `successes_results`): a visualization of the difference between two groups.
#'
#' plot_comparison(`successes_results`, `successes_results`): plots density for the first and the second group.
#'
#' compare_distributions(`successes_results`, `successes_results`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#'
#' plot_distributions(`successes_results`, `successes_results`): a visualization of the distribution for the first group and the second group.
#'
#' plot_distributions_difference(`successes_results`, `successes_results`): a visualization of the difference between the distribution of the first group and the second group.
#'
#' plot_fit(`successes_results`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' traceplot(`successes_results`): traceplot for main fitted model parameters.
#'
#' @exportClass successes_results
successes_results <- setClass(
  "successes_results",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)

#' @exportMethod summary
setMethod(f = "summary", signature(object = "successes_results"), definition = function(object) {
  # get means
  p <- mean(object@extract$p)

  # print
  cat(sprintf("Success rate: %.2f\n", p))
})


#' @title compare
#' @description \code{compare} prints difference in successfulness between two groups.
#' @rdname successes_results-compare
#' @aliases compare,ANY-method
setMethod(f = "compare", signature(object = "successes_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "successes_results") {
    # first group data
    y1 <- rowMeans(object@extract$p)

    # second group data
    object2 <- list(...)[[1]]
    y2 <- rowMeans(object2@extract$p)

    shared_difference(y1, y2)
  } else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(successes_results, successes_results) is required!")
  }
})


#' @title plot_difference
#' @description \code{plot_difference} a visualization of the difference between two groups.
#' @rdname successes_results-plot_difference
#' @aliases plot_difference,ANY-method
setMethod(f = "plot_difference", signature(object = "successes_results"), definition = function(object, ..., bins = 30) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "successes_results") {
    # first group data
    y1 <- rowMeans(object@extract$p)

    # second group data
    object2 <- list(...)[[1]]
    y2 <- rowMeans(object2@extract$p)

    # call plot difference shared function from shared plots
    shared_plot_difference(y1, y2)
  } else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(successes_results, successes_results) is required!")
  }
})


#' @title plot_comparison
#' @description \code{plot_comparison} plots density for the first and the second group.
#' @rdname successes_results-plot_comparison
#' @aliases plot_comparison,ANY-method
setMethod(f = "plot_comparison", signature(object = "successes_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "successes_results") {
    # first group data
    df1 <- data.frame(value = rowMeans(object@extract$p))

    # second group data
    object2 <- list(...)[[1]]
    df2 <- data.frame(value = rowMeans(object2@extract$p))

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
    warning("Wrong parameters provided for the plot_difference function, plot_difference(successes_results, successes_results) is required!")
  }
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#' @rdname successes_results-compare_distributions
#' @aliases compare_distributions,ANY-method
setMethod(f = "compare_distributions", signature(object = "successes_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "successes_results") {
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
    warning("Wrong parameters provided for the plot_difference function, plot_difference(successes_results, successes_results) is required!")
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the distribution for the first group and the second group.
#' @rdname reaction_times_results-plot_distributions
#' @aliases plot_distributions,ANY-method
setMethod(f = "plot_distributions", signature(object = "successes_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "successes_results") {
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
    warning("Wrong parameters provided for the plot_difference function, plot_difference(successes_results, successes_results) is required!")
  }
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the second group.
#' @rdname successes_results-plot_distributions_difference
#' @aliases plot_distributions_difference,ANY-method
setMethod(f = "plot_distributions_difference", signature(object = "successes_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "successes_results") {
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
    warning("Wrong parameters provided for the plot_difference function, plot_difference(successes_results, successes_results) is required!")
  }
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @rdname successes_results-plot_fit
#' @aliases plot_fit,ANY-method
setMethod(f = "plot_fit", signature(object = "successes_results"), definition = function(object) {
  df_data <- data.frame(r = object@data$r, subject = object@data$s)

  df_data <- ddply(df_data, ~ subject, summarise, data=mean(r))

  df_data$fit <- colMeans(object@extract$p)

  df_data <- melt(df_data, id = "subject")

  # ncol
  n_col <- ceiling(sqrt(n))

  # density per subject
  graph <- ggplot(df_data, aes(x = variable, y = value, fill = variable)) +
    geom_bar(stat="identity") +
    facet_wrap(~ subject, ncol = n_col) +
    scale_fill_manual(values = c("#3182bd", "#ff4e3f")) +
    ylim(0, 1) +
    theme_minimal() +
    theme(legend.title=element_blank())

  return(graph)
})


#' @title traceplot
#' @description \code{traceplot} traceplot for main fitted model parameters.
#' @rdname reaction_times_results-traceplot
#' @aliases traceplot,ANY-method
setMethod(f = "traceplot", signature(object = "successes_results"), definition = function(object) {
  rstan::traceplot(object@fit, pars = c("p"), inc_warmup = TRUE)
})
