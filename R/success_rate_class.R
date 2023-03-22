#' @title success_rate_class
#' @import dplyr ggplot2
#' @description An S4 class for storing results of successes (true/false) Bayesian model.
#'
#' \strong{Functions}
#'
#' summary(`success_rate_class`): prints a summary of the fit.
#'
#' print(`success_rate_class`): prints a more detailed summary of the fit
#'
#' show(`success_rate_class`): prints a more detailed summary of the fit.
#'
#' plot(`success_rate_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot(`success_rate_class`, subjects='boolean'): plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subjects level (subjects=TRUE) or on the group level (subjects=FALSE).
#'
#' plot_fit(`success_rate_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_fit(`success_rate_class`, subjects='boolean'): plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subjects level (subjects=TRUE) or on the group level (subjects=FALSE).
#'
#' plot_trace(`success_rate_class`): traceplot for main fitted model parameters.
#'
#' get_parameters(`success_rate_class`): returns a dataframe with values of fitted parameters.
#'
#' get_subject_parameters(`success_rate_class`): returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#'
#' compare_means(`success_rate_class`, fit2=`success_rate_class`): returns difference in success rate between two groups. You can also provide the rope parameter.
#'
#' compare_means(`success_rate_class`, fits=`list`): returns difference in success rate between multiple groups. You can also provide the rope parameter.
#'
#' plot_means_difference(`success_rate_class`, fit2=`success_rate_class`): a visualization of the difference between two groups. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_means_difference(`success_rate_class`, fits=`list`): a visualization of the difference between multiple groups. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_means(`success_rate_class`): plots density for the first group means.
#'
#' plot_means(`success_rate_class`, fit2=`success_rate_class`): plots density for the first and the second group means.
#'
#' plot_means(`success_rate_class`, fits=`list`): plots density for multiple
#'
#' compare_distributions(`success_rate_class`, fit2=`success_rate_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter.
#'
#' compare_distributions(`success_rate_class`, fits=`list`): draws and compares samples from distributions of multiple groups. You can also provide the rope parameter.
#'
#' plot_distributions(`success_rate_class`): a visualization of the fitted distribution.
#'
#' plot_distributions(`success_rate_class`, fit2=`success_rate_class`): a visualization of the distribution for two fits.
#'
#' plot_distributions(`success_rate_class`, fits=`list`): a visualization of the distribution for multiple fits.
#'
#' plot_distributions_difference(`success_rate_class`, fit2=`success_rate_class`): a visualization of the difference between the distribution of the first group and the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_distributions_difference(`success_rate_class`, fits=`list`): a visualization of the difference between the distributions of multiple groups. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_fit(`success_rate_class`): plots fitted model against the data. Use this function to explore the quality of your fit. Fit will be plotted on the group level.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
#'
#' @examples
#' \donttest{
#' # priors
#' p_prior <- b_prior(family = "beta", pars = c(1, 1))
#' tau_prior <- b_prior(family = "uniform", pars = c(0, 500))
#'
#' # attach priors to relevant parameters
#' priors <- list(
#'   c("p", p_prior),
#'   c("tau", tau_prior)
#' )
#'
#' # subjects
#' s <- rep(1:5, 20)
#'
#' # generate data and fit
#' data1 <- rbinom(100, size = 1, prob = 0.6)
#' fit1 <- b_success_rate(r = data1, s = s, priors = priors, chains = 1)
#'
#' data2 <- rbinom(100, size = 1, prob = 0.1)
#' fit2 <- b_success_rate(r = data2, s = s, priors = priors, chains = 1)
#'
#' data3 <- rbinom(100, size = 1, prob = 0.5)
#' fit3 <- b_success_rate(r = data3, s = s, priors = priors, chains = 1)
#'
#' data4 <- rbinom(100, size = 1, prob = 0.9)
#' fit4 <- b_success_rate(r = data4, s = s, priors = priors, chains = 1)
#'
#' # fit list
#' fit_list <- list(fit2, fit3, fit4)
#'
#' # a short summary of fitted parameters
#' summary(fit1)
#'
#' # a more detailed summary of fitted parameters
#' print(fit1)
#' show(fit1)
#'
#' # plot the fitted distribution against the data
#' plot(fit1)
#' plot_fit(fit1)
#'
#' # plot the fitted distribution against the data,
#' # plot on the top (group) level
#' plot(fit1, subjects = FALSE)
#' plot_fit(fit1, subjects = FALSE)
#'
#' # traceplot of the fitted parameters
#' plot_trace(fit1)
#'
#' # extract parameter values from the fit
#' parameters <- get_parameters(fit1)
#'
#' # extract parameter values on the bottom (subject) level from the fit
#' subject_parameters <- get_subject_parameters(fit1)
#'
#' # compare means between two fits, use a rope interval
#' compare_means(fit1, fit2 = fit2, rope = 0.05)
#'
#' # compare means between multiple fits
#' compare_means(fit1, fits = fit_list)
#'
#' # visualize difference in means between two fits,
#' # specify number of histogram bins and rope interval
#' plot_means_difference(fit1, fit2 = fit2, bins = 40, rope = 0.05)
#'
#' # visualize difference in means between multiple fits
#' plot_means_difference(fit1, fits = fit_list)
#'
#' # visualize means of a single fit
#' plot_means(fit1)
#'
#' # visualize means of two fits
#' plot_means(fit1, fit2 = fit2)
#'
#' # visualize means of multiple fits
#' plot_means(fit1, fits = fit_list)
#'
#' # draw samples from distributions underlying two fits and compare them,
#' # use a rope interval
#' compare_distributions(fit1, fit2 = fit2, rope = 0.05)
#'
#' # draw samples from distributions underlying multiple fits and compare them
#' compare_distributions(fit1, fits = fit_list)
#'
#' # visualize the distribution underlying a fit
#' plot_distributions(fit1)
#'
#' # visualize distributions underlying two fits
#' plot_distributions(fit1, fit2 = fit2)
#'
#' # visualize distributions underlying multiple fits
#' plot_distributions(fit1, fits = fit_list)
#'
#' # visualize difference between distributions underlying two fits,
#' # use a rope interval
#' plot_distributions_difference(fit1, fit2 = fit2, rope = 0.05)
#'
#' # visualize difference between distributions underlying multiple fits
#' plot_distributions_difference(fit1, fits = fit_list)
#' }
#'
success_rate_class <- setClass(
  "success_rate_class",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)


#' @title summary
#' @description \code{summary} prints a summary of the Bayesian success rate fit.
#' @param object success_rate_class object.
#' @exportMethod summary
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "summary", signature(object = "success_rate_class"), definition = function(object) {
  # get means
  p <- mean(object@extract$p0)
  tau <- mean(object@extract$tau)

  # HDI
  p_hdi <- mcmc_hdi(object@extract$p0)
  tau_hdi <- mcmc_hdi(object@extract$tau)

  # print
  cat(sprintf(
    "Success rate:\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
    p, mcmcse::mcse(object@extract$p0)$se, p_hdi[1], p_hdi[2]
  ))
  cat(sprintf(
    "Tau:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
    tau, mcmcse::mcse(object@extract$tau)$se, tau_hdi[1], tau_hdi[2]
  ))
})


#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian success rate fit.
#' @param object success_rate_class object.
#' @exportMethod show
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "show", signature(object = "success_rate_class"), definition = function(object) {
  # print
  show(object@fit)
})


#' @title plot
#' @description \code{plot} plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subjects level (subjects=TRUE) or on the group level (subjects=FALSE).
#' @param x success_rate_class object.
#' @param y empty dummy variable, ignore this.
#' @param ... subjects - plot fits on a subject level (default = TRUE).
#' @exportMethod plot
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "plot", signature(x = "success_rate_class", y = "missing"), definition = function(x, ...) {
  return(plot_fit(object = x, ...))
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subjects level (subjects=TRUE) or on the group level (subjects=FALSE).
#' @param object success_rate_class object.
#' @param ... subjects - plot fits on a subject level (default = TRUE).
#' @rdname success_rate_class-plot_fit
#' @aliases plot_fit_success_rate
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "plot_fit", signature(object = "success_rate_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  variable <- value <- NULL

  arguments <- list(...)

  # plot on a subject level?
  subjects <- TRUE
  if (!is.null(arguments$subjects)) {
    subjects <- arguments$subjects
  }

  df_data <- data.frame(value = object@data$r, variable = object@data$s)

  if (!subjects) {
    mean_p <- mean(df_data$value)

    df_fit <- data.frame(value = object@extract$p0)

    graph <- ggplot() +
      geom_vline(xintercept = mean_p, color = "#ff4e3f") +
      geom_density(data = df_fit, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
      xlim(0, 1) +
      xlab("success rate")
  } else {
    df_data <- df_data %>%
      group_by(variable) %>%
      summarize(value = mean(value))

    df_fit <- object@extract$p
    colnames(df_fit) <- seq(1:ncol(df_fit))
    df_fit <- reshape::melt(as.data.frame(df_fit), id = NULL)

    # ncol
    n_col <- ceiling(sqrt(nrow(df_data)))

    # density per subject
    graph <- ggplot() +
      geom_vline(data = df_data, aes(xintercept = value), color = "#ff4e3f") +
      geom_density(data = df_fit, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
      xlim(0, 1) +
      facet_wrap(~variable, ncol = n_col) +
      xlab("success rate")
  }

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object success_rate_class object.
#' @rdname success_rate_class-plot_trace
#' @aliases plot_trace_success_rate
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "plot_trace", signature(object = "success_rate_class"), definition = function(object) {
  rstan::traceplot(object@fit, pars = c("p"), inc_warmup = TRUE)
})


#' @title get_parameters
#' @description \code{get_parameters} returns a dataframe with values of fitted parameters.
#' @param object success_rate_class object.
#' @rdname success_rate_class-get_parameters
#' @aliases get_parameters_success_rate_class
#' @return A data frame with parameter values.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "get_parameters", signature(object = "success_rate_class"), definition = function(object) {
  df <- data.frame(
    p = object@extract$p0,
    tau = object@extract$tau
  )

  return(df)
})


#' @title get_subject_parameters
#' @description \code{get_subject_parameters} returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#' @param object success_rate_class object.
#' @rdname success_rate_class-get_subject_parameters
#' @aliases get_subject_parameters_success_rate_class
#' @return A data frame with parameter values.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "get_subject_parameters", signature(object = "success_rate_class"), definition = function(object) {
  df <- data.frame(p = numeric(), subject = numeric())

  n <- length(unique(object@data$s))

  for (i in 1:n) {
    df_subject <- data.frame(
      p = object@extract$p[, i],
      subject = i
    )

    df <- rbind(df, df_subject)
  }

  return(df)
})


#' @title compare_means
#' @description \code{compare_means} prints difference in success rate between two groups or multiple groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, fits - a list of success_rate_class objects, rope - region of practical equivalence.
#' @rdname success_rate_class-compare_means
#' @aliases compare_means_success_rate
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "compare_means", signature(object = "success_rate_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_means function are invalid, compare_means(success_rate_class, fit2=success_rate_class) or compare_means(success_rate_class, fits=list) is required! You can also provide the rope parameter, e.g. compare_means(success_rate_class, fit2=success_rate_class, rope=numeric)."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # data
  y <- list()

  # first group data
  y[[1]] <- object@extract$p0

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y[[2]] <- fit2@extract$p0
  } else if (!is.null(arguments$fits)) {
    # provided a list of fits
    i <- 2
    for (fit in arguments$fits) {
      if (!("success_rate_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid success_rate_class object.")
      }
      y[[i]] <- fit@extract$p0
      i <- i + 1
    }
  } else {
    stop(wrong_arguments)
  }

  n <- length(y)
  comparison_matrix <- matrix(nrow = n, ncol = n)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      cat(sprintf("\n---------- Group %d vs Group %d ----------\n", i, j))
      result <- difference(y1 = y[[i]], y2 = y[[j]], rope = rope, group1 = i, group2 = j)
      comparison_matrix[j, i] <- result[1]
      comparison_matrix[i, j] <- result[2]
      cat("\n")
    }
  }

  # largest/smallest probabilities
  if (n > 2) {
    cat("-----------------------------------------")
    cat("\nProbabilities that a certain group is\nsmallest/largest or equal to all others:\n\n")
    smallest_largest <- is_smallest_or_largest(data = y, rope = rope)
    print(smallest_largest)
    cat("\n\n")
    return(list(comparison_matrix = comparison_matrix, smallest_largest = smallest_largest))
  } else {
    return(comparison_matrix)
  }
})


#' @title plot_means_difference
#' @description \code{plot_means_difference} a visualization of the difference between two groups or multiple groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, fits - a list of success_rate_class objects, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname success_rate_class-plot_means_difference
#' @aliases plot_means_difference_success_rate
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "plot_means_difference", signature(object = "success_rate_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_means_difference function are invalid, plot_means_difference(success_rate_class, fit2=success_rate_class) or plot_means_difference(success_rate_class, fits=list) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_means_difference(success_rate_class, fit2=success_rate_class, rope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # first group data
  y <- list()
  y[[1]] <- object@extract$p0

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y[[2]] <- fit2@extract$p0
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in arguments$fits) {
      if (!("success_rate_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid success_rate_class object.")
      }
      y[[i]] <- fit@extract$p0

      i <- i + 1
    }
  } else {
    stop(wrong_arguments)
  }

  # bins in the histogram
  bins <- 30
  if (!is.null(arguments$bins)) {
    bins <- arguments$bins
  }

  # if no list is provided
  if (is.null(arguments$fits)) {
    # call plot difference shared function
    graph <- plot_difference(y1 = y[[1]], y2 = y[[2]], rope = rope, bins = bins)
    return(graph)
  } else {
    graphs <- list()
    n <- length(y)
    for (i in 1:n) {
      for (j in i:n) {
        # if both are equal plot means, else plot difference
        if (i == j) {
          df <- data.frame(value = y[[i]])
          index <- (i - 1) * n + i
          graphs[[index]] <- ggplot() +
            geom_density(data = df, aes(x = value), fill = "#3182bd", color = NA, alpha = 0.4) +
            xlab("probability") +
            xlim(0, 1)
        } else {
          index1 <- (i - 1) * n + j
          graphs[[index1]] <- plot_difference(y1 = y[[i]], y2 = y[[j]], rope = rope, bins = bins, nrow = n)

          index2 <- (j - 1) * n + i
          graphs[[index2]] <- plot_difference(y1 = y[[j]], y2 = y[[i]], rope = rope, bins = bins, nrow = n)
        }
      }
    }

    # cowplot
    graph <- suppressWarnings(cowplot::plot_grid(plotlist = graphs, nrow = n, ncol = n, scale = 0.9))
    return(graph)
  }
})


#' @title plot_means
#' @description \code{plot_means} plots density of means for one, two or multiple groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, fits - a list of success_rate_class objects.
#' @rdname success_rate_class-plot_means
#' @aliases plot_means_success_rate
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "plot_means", signature(object = "success_rate_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  group <- value <- NULL

  # first group data
  df <- data.frame(value = object@extract$p0, group = "1")

  # second group data
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }

      df <- rbind(df, data.frame(value = fit2@extract$p0, group = "2"))
    } else if (!is.null(arguments$fits)) {
      i <- 2
      for (fit in arguments$fits) {
        if (!("success_rate_class" %in% class(fit))) {
          stop("One of the fits in the fits list is not a valid success_rate_class object.")
        }
        df <- rbind(df, data.frame(value = fit@extract$p0, group = as.factor(i)))
        i <- i + 1
      }
    }
  }

  # plot
  graph <- ggplot() +
    geom_density(data = df, aes(x = value, fill = group), color = NA, alpha = 0.4) +
    xlab("probability") +
    xlim(0, 1)

  n_groups <- max(as.numeric(df$group))
  if (n_groups == 2) {
    graph <- graph +
      scale_fill_manual(values = c("#3182bd", "#ff4e3f"))
  } else if (n_groups > 2) {
    graph <- graph +
      scale_fill_hue()
  } else {
    graph <- graph +
      scale_fill_manual(values = c("#3182bd")) +
      theme(legend.position = "none")
  }

  return(suppressWarnings(graph))
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group or from samples drawn from distributions of multiple groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, fits - a list of success_rate_class objects, rope - region of practical equivalence.
#' @rdname success_rate_class-compare_distributions
#' @aliases compare_distributions_success_rate
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "compare_distributions", signature(object = "success_rate_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(success_rate_class, fit2=success_rate_class) or compare_distributions(success_rate_class, fits=list) is required!. You can also provide the rope parameter, e.g. compare_distributions(success_rate_class, fit2=success_rate_class, rope=numeric)."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # first group data
  y <- list()
  n <- 100000
  p0 <- mean(object@extract$p0)
  tau <- mean(object@extract$tau)
  y[[1]] <- stats::rbeta(n, p0 * tau, (1 - p0) * tau)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    p0 <- mean(fit2@extract$p0)
    tau <- mean(fit2@extract$tau)
    y[[2]] <- stats::rbeta(n, p0 * tau, (1 - p0) * tau)
  } else if (!is.null(arguments$fits)) {
    # provided a list of fits
    i <- 2
    for (fit in arguments$fits) {
      if (!("success_rate_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid success_rate_class object.")
      }
      p0 <- mean(fit@extract$p0)
      tau <- mean(fit@extract$tau)
      y[[i]] <- stats::rbeta(n, p0 * tau, (1 - p0) * tau)
      i <- i + 1
    }
  } else {
    stop(wrong_arguments)
  }

  n <- length(y)
  comparison_matrix <- matrix(nrow = n, ncol = n)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      cat(sprintf("\n---------- Group %d vs Group %d ----------\n", i, j))
      result <- difference(y1 = y[[i]], y2 = y[[j]], rope = rope, group1 = i, group2 = j)
      comparison_matrix[j, i] <- result[1]
      comparison_matrix[i, j] <- result[2]
      cat("\n")
    }
  }

  # largest/smallest probabilities
  if (n > 2) {
    cat("-----------------------------------------")
    cat("\nProbabilities that a certain group is\nsmallest/largest or equal to all others:\n\n")
    smallest_largest <- is_smallest_or_largest(data = y, rope = rope)
    print(smallest_largest)
    cat("\n\n")
    return(list(comparison_matrix = comparison_matrix, smallest_largest = smallest_largest))
  } else {
    return(comparison_matrix)
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the fitted distribution, for one, two or multiple fits.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, fits - a list of success_rate_class objects.
#' @rdname success_rate_class-plot_distributions
#' @aliases plot_distributions_success_rate
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "plot_distributions", signature(object = "success_rate_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  group <- x <- y <- NULL

  # first group data
  alphas <- vector()
  betas <- vector()
  p0 <- mean(object@extract$p0)
  tau <- mean(object@extract$tau)
  alphas[[1]] <- p0 * tau
  betas[[1]] <- (1 - p0) * tau

  # second group data
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      p0 <- mean(fit2@extract$p0)
      tau <- mean(fit2@extract$tau)
      alphas[[2]] <- p0 * tau
      betas[[2]] <- (1 - p0) * tau
    } else if (!is.null(arguments$fits)) {
      i <- 2
      for (fit in arguments$fits) {
        if (!("success_rate_class" %in% class(fit))) {
          stop("One of the fits in the fits list is not a valid success_rate_class object.")
        }
        p0 <- mean(fit@extract$p0)
        tau <- mean(fit@extract$tau)
        alphas[[i]] <- p0 * tau
        betas[[i]] <- (1 - p0) * tau
        i <- i + 1
      }
    }
  }

  # calculate data points
  step <- 1 / 1000
  df <- data.frame(x = numeric(), y = numeric(), group = factor())
  n_groups <- length(alphas)
  for (i in 1:n_groups) {
    df_group <- data.frame(
      x = seq(0, 1, step),
      y = stats::dbeta(seq(0, 1, step),
        shape1 = alphas[i],
        shape2 = betas[i]
      ),
      group = as.factor(i)
    )

    df <- rbind(df, df_group)
  }

  # plot
  graph <- ggplot() +
    geom_area(data = df, aes(x = x, y = y, fill = group), alpha = 0.4, position = "identity") +
    xlab("probability") +
    ylab("density")

  if (n_groups == 2) {
    graph <- graph +
      scale_fill_manual(values = c("#3182bd", "#ff4e3f"))
  } else if (n_groups > 2) {
    graph <- graph +
      scale_fill_hue()
  } else {
    graph <- graph +
      scale_fill_manual(values = c("#3182bd")) +
      theme(legend.position = "none")
  }

  return(suppressWarnings(graph))
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the second group or between multiple groups.
#' @param object success_rate_class object.
#' @param ... fit2 - a second success_rate_class object, fits - a list of success_rate_class objects, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname success_rate_class-plot_distributions_difference
#' @aliases plot_distributions_difference_success_rate
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?success_rate_class
#'
setMethod(f = "plot_distributions_difference", signature(object = "success_rate_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(success_rate_class, fit2=success_rate_class) or plot_distributions_difference(success_rate_class, fits=list) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_distributions_difference(success_rate_class, fit2=success_rate_class, rope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # first group data
  y <- list()
  n <- 100000
  p0 <- mean(object@extract$p0)
  tau <- mean(object@extract$tau)
  y[[1]] <- stats::rbeta(n, p0 * tau, (1 - p0) * tau)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "success_rate_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    p0 <- mean(fit2@extract$p0)
    tau <- mean(fit2@extract$tau)
    y[[2]] <- stats::rbeta(n, p0 * tau, (1 - p0) * tau)
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in arguments$fits) {
      if (!("success_rate_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid success_rate_class object.")
      }
      p0 <- mean(fit@extract$p0)
      tau <- mean(fit@extract$tau)
      y[[i]] <- stats::rbeta(n, p0 * tau, (1 - p0) * tau)
      i <- i + 1
    }
  } else {
    stop(wrong_arguments)
  }

  # bins in the histogram
  bins <- 30
  if (!is.null(arguments$bins)) {
    bins <- arguments$bins
  }

  # if no list is provided
  if (is.null(arguments$fits)) {
    # call plot difference shared function
    graph <- plot_difference(y1 = y[[1]], y2 = y[[2]], rope = rope, bins = bins)
    return(graph)
  } else {
    graphs <- list()
    n <- length(y)
    for (i in 1:n) {
      for (j in i:n) {
        # if both are equal plot samples, else plot difference
        if (i == j) {
          df <- data.frame(value = y[[i]])
          index <- (i - 1) * n + i
          graphs[[index]] <- ggplot() +
            geom_density(data = df, aes(x = value), fill = "#3182bd", color = NA, alpha = 0.4) +
            xlab("probability") +
            xlim(0, 1)
        } else {
          index1 <- (i - 1) * n + j
          graphs[[index1]] <- plot_difference(y1 = y[[i]], y2 = y[[j]], rope = rope, bins = bins, nrow = n)

          index2 <- (j - 1) * n + i
          graphs[[index2]] <- plot_difference(y1 = y[[j]], y2 = y[[i]], rope = rope, bins = bins, nrow = n)
        }
      }
    }

    # cowplot
    graph <- suppressWarnings(cowplot::plot_grid(plotlist = graphs, nrow = n, ncol = n, scale = 0.9))
    return(graph)
  }
})
