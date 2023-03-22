#' @title ttest_class
#' @import ggplot2
#' @description An S4 class for storing results of Bayesian t-test results.
#'
#' \strong{Functions}
#'
#' summary(`ttest_class`): prints a summary of the fit.
#'
#' print(`ttest_class`): prints a more detailed summary of the fit
#'
#' show(`ttest_class`): prints a more detailed summary of the fit.
#'
#' plot(`ttest_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_fit(`ttest_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_trace(`ttest_class`): traceplot for main fitted model parameters.
#'
#' get_parameters(`ttest_class`): returns a dataframe with values of fitted parameters.
#'
#' compare_means(`ttest_class`, fit2=`ttest_class`): prints difference/equality of the first group against the second group. You can also provide the rope parameter or execute the comparison through the sigma or nu parameter.
#'
#' compare_means(`ttest_class`, mu=`numeric`): prints difference/equality of the first group against a mean value. You can also provide the rope parameter or execute the comparison through the sigma parameter.
#'
#' compare_means(`ttest_class`, mu=`numeric`, sigma=`numeric`): prints difference/equality of the first group against a normal distribution provided with mean value and standard deviation. Note here that sigma is used only in the Cohen's d calculation. You can also provide the rope parameter or execute the comparison through the sigma or nu parameter.
#'
#' compare_means(`ttest_class`, fits=`list`): prints difference/equality of the first group and multiple other groups. You can also provide the rope parameter or execute the comparison through the sigma or nu parameter.
#'
#' plot_means_difference(`ttest_class`, fit2=`ttest_class`): a visualization of the difference between the first group and the second group. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison through the sigma or nu parameter.
#'
#' plot_means_difference(`ttest_class`, mu=`numeric`): a visualization of the difference between the first group and a constant value or a normal distribution with mean value mu. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison through the sigma or nu parameter.
#'
#' plot_means_difference(`ttest_class`, fits=`list`): a visualization of the difference between multiple groups. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison through the sigma or nu parameter.
#'
#' plot_means(`ttest_class`): plots density of means. You can also visualize the density for the sigma or nu parameter.
#'
#' plot_means(`ttest_class`, fit2=`ttest_class`): plots density for the first and the second group means. You can also visualize the density for the sigma or nu parameter.
#'
#' plot_means(`ttest_class`, mu=`numeric`): plots density for the first group means and a mean value in case second group is defined as a normal distribution or as a constant. You can also visualize the density for the sigma or nu parameter.
#'
#' plot_means(`ttest_class`, fits=`list`): plots density for the first group means and means for multiple other groups. You can also visualize the density for the sigma or nu parameter.
#'
#' compare_distributions(`ttest_class`, fit2=`ttest_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, mu=`numeric`): draws samples from distribution of the first group and compares them against a mean value. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, mu=`numeric`, sigma=`numeric`): draws samples from distribution of the first group and compares them against samples from a normal distribution with a defined mean value and variance. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, fits=`list`): draws samples from distribution of the first group and compares them against samples drawn from multiple other groups. You can also provide the rope parameter.
#'
#' plot_distributions(`ttest_class`): a visualization of the fitted distribution.
#'
#' plot_distributions(`ttest_class`, fit2=`ttest_class`): a visualization of two fitted distributions.
#'
#' plot_distributions(`ttest_class`, mu=`numeric`): a visualization of the fitted distribution and a constant value.
#'
#' plot_distributions(`ttest_class`, mu=`numeric`, sigma=`numeric`): a visualization of the fitted distribution and the normal distribution defined with a mean value and a standard deviation.
#'
#' plot_distributions(`ttest_class`, fits=`list`): a visualization of multiple fitted distributions.
#'
#' plot_distributions_difference(`ttest_class`, fit2=`ttest_class`): a visualization of the difference between the distribution of the first group and the distribution of the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_distributions_difference(`ttest_class`, mu=`numeric`): a visualization of the difference between the distribution of the first group and a constant value. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_distributions_difference(`ttest_class`, mu=`numeric`, sigma=`numeric`): a visualization of the difference between the distribution of the first group and the normal distribution defined with a mean value and standard deviation. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_distributions_difference(`ttest_class`, fits=`list`): a visualization of the difference between multiple groups. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Raw data for the tested group.
#'
#' @examples
#' \donttest{
#' # priors
#' mu_prior <- b_prior(family = "normal", pars = c(0, 1000))
#' sigma_prior <- b_prior(family = "uniform", pars = c(0, 500))
#' nu_prior <- b_prior(family = "normal", pars = c(2000, 1000))
#'
#' # attach priors to relevant parameters
#' priors <- list(
#'   c("mu", mu_prior),
#'   c("sigma", sigma_prior),
#'   c("nu", nu_prior)
#' )
#'
#' # generate data and fit
#' data1 <- rnorm(20, mean = 150, sd = 20)
#' fit1 <- b_ttest(data = data1, priors = priors, chains = 1)
#'
#' data2 <- rnorm(20, mean = 200, sd = 20)
#' fit2 <- b_ttest(data = data2, priors = priors, chains = 1)
#'
#' data3 <- rnorm(20, mean = 150, sd = 40)
#' fit3 <- b_ttest(data = data3, priors = priors, chains = 1)
#'
#' data4 <- rnorm(20, mean = 50, sd = 10)
#' fit4 <- b_ttest(data = data4, priors = priors, chains = 1)
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
#' # traceplot of the fitted parameters
#' plot_trace(fit1)
#'
#' # extract parameter values from the fit
#' parameters <- get_parameters(fit1)
#'
#' # compare means between two fits
#' compare_means(fit1, fit2 = fit2)
#'
#' # compare means between two fits, use a rope interval
#' compare_means(fit1, fit2 = fit2, rope = 2)
#'
#' # compare means between a fit and a constant value
#' compare_means(fit1, mu = 150)
#'
#' # compare means between a fit and a distribution,
#' # sigma is used for calculating Cohen's d
#' compare_means(fit1, mu = 150, sigma = 20)
#'
#' # compare means between multiple fits
#' compare_means(fit1, fits = fit_list)
#'
#' # visualize difference in means between two fits,
#' # specify number of histogram bins
#' plot_means_difference(fit1, fit2 = fit2, bins = 20)
#'
#' # visualize difference in means between a fit and a constant value
#' plot_means_difference(fit1, mu = 150)
#'
#' # visualize difference in means between multiple fits, use a rope interval
#' plot_means_difference(fit1, fits = fit_list, rope = 2)
#'
#' # visualize means of a single fit
#' plot_means(fit1)
#'
#' # visualize means of two fits
#' plot_means(fit1, fit2 = fit2)
#'
#' # visualize means of a fit and a constant value
#' plot_means(fit1, mu = 150)
#'
#' # visualize means of multiple fits
#' plot_means(fit1, fits = fit_list)
#'
#' # draw samples from distributions underlying two fits and compare them
#' compare_distributions(fit1, fit2 = fit2)
#'
#' # draw samples from a distribution underlying the fit
#' # and compare them with a constant, use a rope interval
#' compare_distributions(fit1, mu = 150, rope = 2)
#'
#' # draw samples from a distribution underlying the fit and
#' # compare them with a user defined distribution
#' compare_distributions(fit1, mu = 150, sigma = 20)
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
#' # visualize the distribution underlying a fit and a constant value
#' plot_distributions(fit1, mu = 150)
#'
#' # visualize the distribution underlying a fit and a user defined distribution
#' plot_distributions(fit1, mu = 150, sigma = 20)
#'
#' # visualize distributions underlying multiple fits
#' plot_distributions(fit1, fits = fit_list)
#'
#' # visualize difference between distributions underlying two fits,
#' # use a rope interval
#' plot_distributions_difference(fit1, fit2 = fit2, rope = 2)
#'
#' # visualize difference between a distribution underlying the fit
#' # and a constant value
#' plot_distributions_difference(fit1, mu = 150)
#'
#' # visualize difference between a distribution underlying the fits
#' # and a user defined distribution
#' plot_distributions_difference(fit1, mu = 150, sigma = 20)
#'
#' # visualize difference between distributions underlying multiple fits
#' plot_distributions_difference(fit1, fits = fit_list)
#' }
#'
ttest_class <- setClass(
  "ttest_class",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "numeric"
  ),
  contains = "b_results"
)

#' @title summary
#' @description \code{summary} prints a summary of the Bayesian ttest fit.
#' @param object ttest_class object.
#' @exportMethod summary
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "summary", signature(object = "ttest_class"), definition = function(object) {
  # get means
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)
  nu <- mean(object@extract$nu)

  # HDI
  mu_hdi <- mcmc_hdi(object@extract$mu)
  sigma_hdi <- mcmc_hdi(object@extract$sigma)
  nu_hdi <- mcmc_hdi(object@extract$nu)

  # print)
  cat(sprintf(
    "mu:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
    mu, mcmcse::mcse(object@extract$mu)$se, mu_hdi[1], mu_hdi[2]
  ))
  cat(sprintf(
    "sigma:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
    sigma, mcmcse::mcse(object@extract$sigma)$se, sigma_hdi[1], sigma_hdi[2]
  ))
  cat(sprintf(
    "nu:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n", nu,
    mcmcse::mcse(object@extract$nu)$se, nu_hdi[1], nu_hdi[2]
  ))
})


#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian ttest fit.
#' @param object ttest_class object.
#' @exportMethod show
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "show", signature(object = "ttest_class"), definition = function(object) {
  # print
  show(object@fit)
})


#' @title plot
#' @description \code{plot} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param x ttest_class object.
#' @exportMethod plot
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "plot", signature(x = "ttest_class", y = "missing"), definition = function(x) {
  return(plot_fit(object = x))
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object ttest_class object.
#' @rdname ttest_class-plot_fit
#' @aliases plot_fit_ttest
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "plot_fit", signature(object = "ttest_class"), definition = function(object) {
  # init local varibales for CRAN check
  value <- NULL

  df_data <- data.frame(value = object@data)

  nu <- mean(object@extract$nu)
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)

  # get x range
  x_min <- min(mu - 4 * sigma, df_data$value)
  x_max <- max(mu + 4 * sigma, df_data$value)

  diff <- x_max - x_min
  x_min <- x_min - 0.1 * diff
  x_max <- x_max + 0.1 * diff

  df_x <- data.frame(x = c(x_min, x_max))

  graph <- ggplot(data = df_x) +
    geom_density(data = df_data, aes(x = value), fill = "#3182bd", alpha = 0.4, color = NA) +
    stat_function(fun = metRology::dt.scaled, n = 10000, args = list(df = nu, mean = mu, sd = sigma), colour = "#3182bd", size = 1) +
    xlab("value") +
    xlim(x_min, x_max)

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object ttest_class object.
#' @rdname ttest_class-plot_trace
#' @aliases plot_trace_ttest
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "plot_trace", signature(object = "ttest_class"), definition = function(object) {
  traceplot(object@fit, pars = c("mu", "sigma", "nu"), inc_warmup = TRUE)
})


#' @title get_parameters
#' @description \code{get_parameters} returns a dataframe with values of fitted parameters.
#' @param object ttest_class object.
#' @rdname ttest_class-get_parameters
#' @aliases get_parameters_ttest_class
#' @return A data frame with parameter values.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "get_parameters", signature(object = "ttest_class"), definition = function(object) {
  df <- data.frame(
    mu = object@extract$mu,
    sigma = object@extract$sigma,
    nu = object@extract$nu
  )

  return(df)
})


#' @title compare_means
#' @description \code{compare_means} prints difference/equality of the first group against the second group, against multiple groups, against a mean value or against a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, sigma - standard deviation, fits - a list of ttest_class objects, rope - region of practical equivalence, par - execute comparison through the sigma or nu parameter.
#' @rdname ttest_class-compare_means
#' @aliases compare_means_ttest
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "compare_means", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_means function are invalid, compare_means(ttest_class, fit2=ttest_class), compare_means(ttest_class, mu=numeric), compare_means(ttest_class, mu=numeric, sigma=numeric) or compare_means(ttest_class, fits=list) is required! You provide the rope parameter, e.g. compare_means(ttest_class, fit2=ttest_class, rope=numeric) or execute the comparison through the sigma or nu parameter, e.g. compare_means(ttest_class, fit2=ttest_class, par=\"sigma\")."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # compare through a different parameter
  par <- "mu"
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "mu" || par == "sigma" || par == "nu")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are mu, sigma and nu! Using the default setting for comparison.", par)
      warning(w)
      par <- "mu"
    } else {
      cat(sprintf("\n---------- Using the %s parameter. ----------\n\n", par))
    }
  }

  # data
  y <- list()

  # first group data
  if (par == "mu") {
    y[[1]] <- object@extract$mu
  } else if (par == "sigma") {
    y[[1]] <- object@extract$sigma
  } else if (par == "nu") {
    y[[1]] <- object@extract$nu
  }


  sigma1 <- mean(object@extract$sigma)
  n <- length(y[[1]])

  # second group data
  sigma2 <- NULL
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }

    if (par == "mu") {
      y[[2]] <- fit2@extract$mu
    } else if (par == "sigma") {
      y[[2]] <- fit2@extract$sigma
    } else if (par == "nu") {
      y[[2]] <- fit2@extract$nu
    }

    sigma2 <- mean(fit2@extract$sigma)
  } else if (!is.null(arguments$mu) && par == "mu") {
    # provided mu
    y[[2]] <- arguments$mu

    # provided also sigma?
    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
    }
  } else if (!is.null(arguments$sigma) && par == "sigma") {
    # provided sigma
    y[[2]] <- arguments$sigma
    sigma2 <- arguments$sigma
  } else if (!is.null(arguments$fits)) {
    # provided a list of fits
    i <- 2
    for (fit in arguments$fits) {
      if (!("ttest_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid ttest_class object.")
      }

      if (par == "mu") {
        y[[i]] <- fit@extract$mu
      } else if (par == "sigma") {
        y[[i]] <- fit@extract$sigma
      } else if (par == "nu") {
        y[[i]] <- fit@extract$nu
      }

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

  # add Cohen's d if 2 groups are provided
  if (!is.null(sigma2)) {
    diff <- mean(y[[1]]) - mean(y[[2]])
    cohens_d <- diff / sqrt((n * sigma1^2 + n * sigma2^2) / (n + n - 2))
    cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
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
#' @description \code{plot_means_difference} a visualization of the difference of the first group against the second group, against multiple groups, against a mean value or against a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, fits - a list of ttest_class objects, mu - mean value, rope - region of practical equivalence, bins - number of bins in the histogram, par - compare through the sigma or nu parameter.
#' @rdname ttest_class-plot_means_difference
#' @aliases plot_means_difference_ttest
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "plot_means_difference", signature(object = "ttest_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_means_difference function are invalid, plot_means_difference(ttest_class, fit2=ttest_class), plot_means_difference(ttest_class, fits=list) or plot_means_difference(ttest_class, mu=numeric) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_means_difference(ttest_class, fit2=ttest_class, rope=numeric, bins=numeric) or visualise the difference of the sigma or nu parameter, e.g. plot_means_difference(ttest_class, fit2=ttest_class, par=\"sigma\")."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # compare through a different parameter
  par <- "mu"
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "mu" || par == "sigma" || par == "nu")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are mu, sigma and nu! Using the default setting for comparison.", par)
      warning(w)
      par <- "mu"
    } else {
      cat(sprintf("\n---------- Using the %s parameter. ----------\n\n", par))
    }
  }

  # data
  y <- list()

  # first group data
  if (par == "mu") {
    y[[1]] <- object@extract$mu
  } else if (par == "sigma") {
    y[[1]] <- object@extract$sigma
  } else if (par == "nu") {
    y[[1]] <- object@extract$nu
  }

  # limits
  x_min <- min(y[[1]])
  x_max <- max(y[[1]])

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }

    if (par == "mu") {
      y[[2]] <- fit2@extract$mu
    } else if (par == "sigma") {
      y[[2]] <- fit2@extract$sigma
    } else if (par == "nu") {
      y[[2]] <- fit2@extract$nu
    }
  } else if (!is.null(arguments$mu) && par == "mu") {
    y[[2]] <- arguments$mu
  } else if (!is.null(arguments$sigma) && par == "sigma") {
    y[[2]] <- arguments$sigma
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in arguments$fits) {
      if (!("ttest_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid ttest_class object.")
      }

      if (par == "mu") {
        y[[i]] <- fit@extract$mu
      } else if (par == "sigma") {
        y[[i]] <- fit@extract$sigma
      } else if (par == "nu") {
        y[[i]] <- fit@extract$nu
      }

      # limits
      x_min <- min(x_min, y[[i]])
      x_max <- max(x_max, y[[i]])

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
    diff <- x_max - x_min
    x_min <- x_min - 0.1 * diff
    x_max <- x_max + 0.1 * diff

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
            xlab("value") +
            xlim(x_min, x_max)
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
#' @description \code{plot_means} plots density of means, the first and the second group means, means of multiple groups or a mean value in case second group is defined as a constant.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, fits - a list of ttest_class objects, par - plot the sigma or nu parameter.
#' @rdname ttest_class-plot_means
#' @aliases plot_means_ttest
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "plot_means", signature(object = "ttest_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  group <- value <- NULL

  # compare through a different parameter
  arguments <- list(...)
  par <- "mu"
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "mu" || par == "sigma" || par == "nu")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are mu, sigma and nu! Using the default setting for comparison.", par)
      warning(w)
      par <- "mu"
    } else {
      cat(sprintf("\n---------- Using the %s parameter. ----------\n\n", par))
    }
  }

  # first group data
  df <- NULL
  if (par == "mu") {
    df <- data.frame(value = object@extract$mu, group = "1")
  } else if (par == "sigma") {
    df <- data.frame(value = object@extract$sigma, group = "1")
  } else if (par == "nu") {
    df <- data.frame(value = object@extract$nu, group = "1")
  }

  # second group data
  par2 <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }

      if (par == "mu") {
        df <- rbind(df, data.frame(value = fit2@extract$mu, group = "2"))
      } else if (par == "sigma") {
        df <- rbind(df, data.frame(value = fit2@extract$sigma, group = "2"))
      } else if (par == "nu") {
        df <- rbind(df, data.frame(value = fit2@extract$nu, group = "2"))
      }
    } else if (!is.null(arguments$mu) && par == "mu") {
      # provided mu and sigma
      par2 <- arguments$mu
    } else if (!is.null(arguments$sigma) && par == "sigma") {
      # provided mu and sigma
      par2 <- arguments$sigma
    } else if (!is.null(arguments$fits)) {
      i <- 2
      for (fit in arguments$fits) {
        if (!("ttest_class" %in% class(fit))) {
          stop("One of the fits in the fits list is not a valid ttest_class object.")
        }

        if (par == "mu") {
          df <- rbind(df, data.frame(value = fit@extract$mu, group = as.factor(i)))
        } else if (par == "sigma") {
          df <- rbind(df, data.frame(value = fit@extract$sigma, group = as.factor(i)))
        } else if (par == "nu") {
          df <- rbind(df, data.frame(value = fit@extract$nu, group = as.factor(i)))
        }

        i <- i + 1
      }
    }
  }

  # plot
  graph <- ggplot() +
    geom_density(data = df, aes(x = value, fill = group), color = NA, alpha = 0.4) +
    xlab("value")

  n_groups <- max(as.numeric(df$group))
  if (n_groups == 2) {
    graph <- graph +
      scale_fill_manual(values = c("#3182bd", "#ff4e3f"))
  } else if (n_groups > 2) {
    graph <- graph +
      scale_fill_hue()
  } else if (n_groups == 1 & !is.null(par2)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = par2, xend = par2, y = 0, yend = y_max[2] * 1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", par2), x = par2, y = y_max[2] * 1.08), size = 4) +
      scale_fill_manual(values = c("#3182bd")) +
      theme(legend.position = "none")
  } else {
    graph <- graph +
      scale_fill_manual(values = c("#3182bd")) +
      theme(legend.position = "none")
  }

  # limits
  x_min <- min(df$value)
  x_max <- max(df$value)
  diff <- x_max - x_min

  # par2 (mu or sigma)
  if (!is.null(par2)) {
    x_min <- min(x_min, par2)
    x_max <- max(x_max, par2)
  }

  diff <- x_max - x_min
  x_min <- x_min - 0.1 * diff
  x_max <- x_max + 0.1 * diff

  graph <- graph + xlim(x_min, x_max)

  return(suppressWarnings(graph))
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group, against samples drawn from distributions of multiple groups, against a mean value or against samples from a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, fits - a list of ttest_class objects, mu - mean value, sigma - standard deviation, rope - region of practical equivalence.
#' @rdname ttest_class-compare_distributions
#' @aliases compare_distributions_ttest
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "compare_distributions", signature(object = "ttest_class"), definition = function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(ttest_class, fit2=ttest_class), compare_distributions(ttest_class, fits=list), compare_distributions(ttest_class, mu=numeric) or compare_distributions(ttest_class, mu=numeric, sigma=numeric) is required! You can also provide the rope parameter, e.g. compare_distributions(ttest_class, fit2=ttest_class, rope=numeric)."

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
  n <- 100000
  y <- NULL
  sigma1 <- mean(object@extract$sigma)
  y[[1]] <- metRology::rt.scaled(n,
    df = mean(object@extract$nu),
    mean = mean(object@extract$mu),
    sd = sigma1
  )

  # second group data
  sigma2 <- 0
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    sigma2 <- mean(fit2@extract$sigma)

    y[[2]] <- metRology::rt.scaled(n,
      df = mean(fit2@extract$nu),
      mean = mean(fit2@extract$mu),
      sd = sigma2
    )
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
      y[[2]] <- stats::rnorm(n, arguments$mu, sigma2)
    } else {
      y[[2]] <- stats::rnorm(n, arguments$mu, 0)
    }
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in arguments$fits) {
      if (!("ttest_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid ttest_class object.")
      }

      y[[i]] <- metRology::rt.scaled(n,
        df = mean(fit@extract$nu),
        mean = mean(fit@extract$mu),
        sd = mean(fit@extract$sigma)
      )

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

  # add Cohen's d if 2 groups are provided
  if (!is.null(sigma2)) {
    diff <- mean(y[[1]]) - mean(y[[2]])
    cohens_d <- diff / sqrt((n * sigma1^2 + n * sigma2^2) / (n + n - 2))
    cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
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
#' @description \code{plot_distributions} visualizes distributions underlying tested groups.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, fits - a list of ttest_class objects, mu - mean value, sigma - standard deviation.
#' @rdname ttest_class-plot_distributions
#' @aliases plot_distributions_ttest
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "plot_distributions", signature(object = "ttest_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  x <- y <- group <- NULL

  # first group data
  nus <- vector()
  nus[1] <- mean(object@extract$nu)
  mus <- vector()
  mus[1] <- mean(object@extract$mu)
  sigmas <- vector()
  sigmas[1] <- mean(object@extract$sigma)

  # second group data
  mu2 <- NULL
  sigma2 <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      nus[2] <- mean(fit2@extract$nu)
      mus[2] <- mean(fit2@extract$mu)
      sigmas[2] <- mean(fit2@extract$sigma)
    } else if (!is.null(arguments$mu)) {
      # provided mu and/or sigma
      mu2 <- arguments$mu

      if (!is.null(arguments$sigma)) {
        sigma2 <- arguments$sigma
      }
    } else if (!is.null(arguments$fits)) {
      i <- 2
      for (fit in arguments$fits) {
        if (!("ttest_class" %in% class(fit))) {
          stop("One of the fits in the fits list is not a valid ttest_class object.")
        }

        nus[i] <- mean(fit@extract$nu)
        mus[i] <- mean(fit@extract$mu)
        sigmas[i] <- mean(fit@extract$sigma)
        i <- i + 1
      }
    }
  }

  # get boundaries
  x_min <- min(mus - 4 * sigmas)
  x_max <- max(mus + 4 * sigmas)

  if (!is.null(mu2) & !is.null(sigma2)) {
    x_min <- min(x_min, mu2 - 4 * sigma2)
    x_max <- max(x_max, mu2 + 4 * sigma2)
  } else if (!is.null(mu2)) {
    if (mu2 < x_min) {
      x_min <- mu2
      x_min <- x_min - (0.1 * (x_max - x_min))
    } else if (mu2 > x_max) {
      x_max <- mu2
      x_max <- x_max + (0.1 * (x_max - x_min))
    }
  }

  # calculate data points
  step <- (x_max - x_min) / 1000
  df <- data.frame(x = numeric(), y = numeric(), group = factor())
  n_groups <- length(mus)
  for (i in 1:n_groups) {
    df_group <- data.frame(
      x = seq(x_min, x_max, step),
      y = metRology::dt.scaled(seq(x_min, x_max, step),
        df = nus[i],
        mean = mus[i],
        sd = sigmas[i]
      ),
      group = as.factor(i)
    )

    df <- rbind(df, df_group)
  }

  if (!is.null(mu2) & !is.null(sigma2)) {
    df_group <- data.frame(
      x = seq(x_min, x_max, step),
      y = stats::dnorm(seq(x_min, x_max, step),
        mean = mu2,
        sd = sigma2
      ),
      group = "2"
    )

    df <- rbind(df, df_group)

    n_groups <- 2
  }

  # plot
  graph <- ggplot() +
    geom_area(data = df, aes(x = x, y = y, fill = group), alpha = 0.4, position = "identity") +
    xlab("value") +
    ylab("density")

  if (n_groups == 2) {
    graph <- graph +
      scale_fill_manual(values = c("#3182bd", "#ff4e3f"))
  } else if (n_groups > 2) {
    graph <- graph +
      scale_fill_hue()
  } else if (n_groups == 1 & !is.null(mu2)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x = mu2, xend = mu2, y = 0, yend = y_max[2] * 1.05), size = 1.5, color = "#ff4e3f", alpha = 0.4) +
      geom_text(aes(label = sprintf("%.2f", mu2), x = mu2, y = y_max[2] * 1.08), size = 4) +
      scale_fill_manual(values = c("#3182bd")) +
      theme(legend.position = "none")
  } else {
    graph <- graph +
      scale_fill_manual(values = c("#3182bd")) +
      theme(legend.position = "none")
  }

  return(suppressWarnings(graph))
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group, the distribution or a constant value for the second group or between multiple distributions.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, fits - a list of ttest_class objects, mu - mean value, sigma - standard deviation, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname ttest_class-plot_distributions_difference
#' @aliases plot_distributions_difference_ttest
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?ttest_class
#'
setMethod(f = "plot_distributions_difference", signature(object = "ttest_class"), definition = function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(ttest_class, fit2=ttest_class), plot_distributions_difference(ttest_class, mu=numeric), plot_distributions_difference(ttest_class, mu=numeric, sigma=numeric) or plot_distributions_difference(ttest_class, fits=list) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_distributions_difference(ttest_class, fit2=ttest_class, rope=numeric, bins=numeric)."

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
  y[[1]] <- metRology::rt.scaled(n,
    df = mean(object@extract$nu),
    mean = mean(object@extract$mu),
    sd = mean(object@extract$sigma)
  )

  # limits
  x_min <- min(y[[1]])
  x_max <- max(y[[1]])

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y[[2]] <- metRology::rt.scaled(n,
      df = mean(fit2@extract$nu),
      mean = mean(fit2@extract$mu),
      sd = mean(fit2@extract$sigma)
    )
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    sigma2 <- 0

    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
    }

    y[[2]] <- stats::rnorm(n, arguments$mu, sigma2)
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in arguments$fits) {
      if (!("ttest_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid ttest_class object.")
      }

      y[[i]] <- metRology::rt.scaled(n,
        df = mean(fit@extract$nu),
        mean = mean(fit@extract$mu),
        sd = mean(fit@extract$sigma)
      )

      # limits
      x_min <- min(x_min, y[[i]])
      x_max <- max(x_max, y[[i]])

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
    diff <- x_max - x_min
    x_min <- x_min - 0.1 * diff
    x_max <- x_max + 0.1 * diff

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
            xlab("value") +
            xlim(x_min, x_max)
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
