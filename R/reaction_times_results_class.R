#' An S4 class for storing results of reaction time Bayesian model.
#' @slot extract Extract from Stan fit.
#' @slot data Data on which the fit is based.
#' @examples
#' summary(`reaction_times_results`): prints summary od the fit.
#'
#' plot_fit(`reaction_times_results`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_difference(`reaction_times_results`, `reaction_times_results`): a visualization of the difference between two groups.
#'
#' @exportClass reaction_times_results
reaction_times_results <- setClass(
  "reaction_times_results",
  #contains = "b_results",
  slots = c(
    extract = "list",
    data = "list"
  ),
  contains = "b_results"
)

#' @exportMethod summary
setMethod(f = "summary", signature(object = "reaction_times_results"), definition = function(object) {
  # get means
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)
  lambda <- mean(object@extract$lambda)
  p <- mean(object@extract$p)

  # print
  cat("mu: ", mu, "\n")
  cat("sigma: ", sigma, "\n")
  cat("lambda: ", lambda, "\n")
  cat("Success rate: ", p, "\n")
})


#' @rdname reaction_times_results-plot_fit
#' @exportMethod plot_fit
setGeneric(name = "plot_fit", function(object) standardGeneric("plot_fit"))

#' @title plot_fit
#' @description \code{plot_fit} visualizes fitted model against the data.
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
                     y = demg(seq(0, 2, 0.01),
                              mu = mean(object@extract$mu[,i]),
                              sigma = mean(object@extract$sigma[,i]),
                              lambda = mean(object@extract$lambda[,i])))

    df_fit <- rbind(df_fit, df)
  }

  # ncol
  n_col <- ceiling(sqrt(n))

  # density per subject
  graph <- ggplot(df_data, aes(x = rt)) +
    geom_density(fill = "#3182bd", alpha = 0.3, color = NA) +
    geom_line(data = df_fit, aes(x = x, y = y)) +
    facet_wrap(~ s, ncol = n_col) +
    xlab("Reaction time") +
    theme_minimal()

  graph
})

#' @title plot_difference
#' @description \code{plot_difference} visualizes difference/equality of two tested groups.
#' @rdname reaction_times_results-plot_difference
#' @aliases plot_difference,ANY-method
setMethod(f = "plot_difference", signature(object = "reaction_times_results"), definition = function(object, ...) {
  # check if correct amount of parameters and if they are of appropriate type
  if (length(list(...)) == 1 && class(list(...)[[1]])[1] == "reaction_times_results") {
    object2 <- list(...)[[1]]

    # first group data
    y1 <- object@extract$mu_m + 1/object@extract$mu_l

    # second group data
    y2 <- object2@extract$mu_m + 1/object2@extract$mu_l

    # call plot difference shared function from shared plots
    shared_plot_difference(y1, y2)
  }
  else {
    warning("Wrong parameters provided for the plot_difference function, plot_difference(reaction_times_results, reaction_times_results) is required!")
  }
})
