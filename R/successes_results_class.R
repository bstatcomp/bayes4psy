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
      xlab("value") +
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
    p1 <- mean(object@extract$p)
    y1 <- rbinom(n, 1, p1)

    # second group data
    object2 <- list(...)[[1]]
    p2 <- mean(object2@extract$p)
    y2 <- rbinom(n, 1, p2)

    # calculate
    y1_smaller <- round(sum(y1 < y2) / n, 2)
    y1_greater <- round(sum(y1 > y2) / n, 2)
    equal <- 1 - y1_smaller - y1_greater

    # print
    cat(sprintf("Probabilities:\n  - Group 1 < Group 2: %.2f", y1_smaller))
    cat(sprintf("\n  - Group 1 > Group 2: %.2f", y1_greater))
    cat(sprintf("\n  - Equal: %.2f\n", equal))

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
    n <- nrow(object@extract$p)
    m <- 10000

    # first group data
    p1 <- mean(object@extract$p)
    y1 <- rbinom(m, n, p1) / n
    mu1 <- mean(y1)
    sd1 <- sd(y1)

    # second group data
    object2 <- list(...)[[1]]
    p2 <- mean(object2@extract$p)
    y2 <- rbinom(m, n, p2) / n
    df2 <- data.frame(value = y2)
    mu2 <- mean(y2)
    sd2 <- sd(y2)

    # plot
    df_x <- data.frame(value = c(0, 1))

    graph <- ggplot(data = df_x, aes(x = value)) +
      stat_function(fun = dnorm, n = m, args = list(mean = mu1, sd = sd1), geom = 'area', fill = '#3182bd', alpha = 0.4) +
      stat_function(fun = dnorm, n = m, args = list(mean = mu2, sd = sd2), geom = 'area', fill = '#ff4e3f', alpha = 0.4) +
      theme_minimal() +
      xlab("Probability") +
      ylab("Density")

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
    p1 <- mean(object@extract$p)
    y1 <- rbinom(n, 1, p1)

    # second group data
    object2 <- list(...)[[1]]
    p2 <- mean(object2@extract$p)
    y2 <- rbinom(n, 1, p2)

    # calculate
    y1_smaller <- round(sum(y1 < y2) / n, 2)
    y1_greater <- round(sum(y1 > y2) / n, 2)
    equal <- 1 - y1_smaler - y1_greater

    levels <- c("Group 1 > Group 2", "Equal", "Group 1 < Group 2")
    df <- data.frame(x = c(1, 1, 1),
                     variable = factor(levels, levels = levels),
                     value = c(y1_greater, equal, y1_smaller))

    # plot
    graph <- ggplot(data = df, aes(x = x, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#3182bd", "grey80", "#ff4e3f")) +
      theme_minimal() +
      theme(legend.title=element_blank()) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      ylab("Probability") +
      xlim(0.3, 1.7)

    return(graph)

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
