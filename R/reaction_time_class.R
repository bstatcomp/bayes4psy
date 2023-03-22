#' @title reaction_time_class
#' @import ggplot2
#' @description An S4 class for storing results of reaction time Bayesian model.
#'
#' \strong{Functions}
#'
#' summary(`reaction_time_class`): prints a summary of the fit.
#'
#' print(`reaction_time_class`): prints a more detailed summary of the fit
#'
#' show(`reaction_time_class`): prints a more detailed summary of the fit.
#'
#' plot(`reaction_time_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot(`reaction_time_class`, subjects='boolean'): plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subject level (subjects=TRUE) or on the group level (subjects=FALSE).
#'
#' plot_fit(`reaction_time_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_fit(`reaction_time_class`, subjects='boolean'): plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subject level (subjects=TRUE) or on the group level (subjects=FALSE).
#'
#' plot_trace(`reaction_time_class`): traceplot for main fitted model parameters.
#'
#' get_parameters(`reaction_time_class`): returns a dataframe with values of fitted parameters.
#'
#' get_subject_parameters(`reaction_time_class`): returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#'
#' compare_means(`reaction_time_class`, fit2=`reaction_time_class`): returns difference in reaction times between two groups. You can also provide the rope parameter or execute the comparison only through a chosen parameter - mu or lambda.
#'
#' compare_means(`reaction_time_class`, fits=`list`): returns difference in reaction times between multiple groups. You can also provide the rope parameter. You can also provide the rope parameter or execute the comparison only through a chosen parameter - mu or lambda.
#'
#' plot_means_difference(`reaction_time_class`, fit2=`reaction_time_class`): a visualization of the difference between two groups. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through a chosen parameter - mu or lambda.
#'
#' plot_means_difference(`reaction_time_class`, fits=`list`): a visualization of the difference between multiple groups. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through a chosen parameter - mu or lambda.
#'
#' plot_means(`reaction_time_class`): plots density of the means. You can also visualize the density only for a chosen parameter - mu or lambda.
#'
#' plot_means(`reaction_time_class`, fit2=`reaction_time_class`): plots density for the first and the second group means. You can also visualize the density only for a chosen parameter - mu or lambda.
#'
#' plot_means(`reaction_time_class`, fits=`list`): plots density for means of multiple groups. You can also visualize the density only for a chosen parameter - mu or lambda.
#'
#' compare_distributions(`reaction_time_class`, fit2=`reaction_time_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter.
#'
#' compare_distributions(`reaction_time_class`, fits=`lists`): draws and compares samples from distributions of multiple groups. You can also provide the rope parameter.
#'
#' plot_distributions(`reaction_time_class`): a visualization of the fitted distribution.
#'
#' plot_distributions(`reaction_time_class`, fit2=`reaction_time_class`): a visualization of the distribution for two fits.
#'
#' plot_distributions(`reaction_time_class`, fits=`list`): a visualization of the distribution for multiple fits.
#'
#' plot_distributions_difference(`reaction_time_class`, fit2=`reaction_time_class`): a visualization of the difference between the distribution of the first group and the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_distributions_difference(`reaction_time_class`, fits=`list`): a visualization of the difference between the distributions of multiple groups. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
#'
#' @examples
#' \donttest{
#' # priors
#' mu_prior <- b_prior(family="normal", pars=c(0, 100))
#' sigma_prior <- b_prior(family="uniform", pars=c(0, 500))
#' lambda_prior <- b_prior(family="uniform", pars=c(0.05, 5))
#'
#' # attach priors to relevant parameters
#' priors <- list(c("mu_m", mu_prior),
#'                c("sigma_m", sigma_prior),
#'                c("mu_s", sigma_prior),
#'                c("sigma_s", sigma_prior),
#'                c("mu_l", lambda_prior),
#'                c("sigma_l", sigma_prior))
#'
#'
#' # subjects
#' s <- rep(1:5, 20)
#'
#' # generate data and fit
#' rt1 <- emg::remg(100, mu=10, sigma=1, lambda=0.4)
#' fit1 <- b_reaction_time(t=rt1, s=s, priors=priors, chains=1)
#'
#' rt2 <- emg::remg(100, mu=10, sigma=2, lambda=0.1)
#' fit2 <- b_reaction_time(t=rt2, s=s, priors=priors, chains=1)
#'
#' rt3 <- emg::remg(100, mu=20, sigma=2, lambda=1)
#' fit3 <- b_reaction_time(t=rt3, s=s, priors=priors, chains=1)
#'
#' rt4 <- emg::remg(100, mu=15, sigma=2, lambda=0.5)
#' fit4 <- b_reaction_time(t=rt4, s=s, priors=priors, chains=1)
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
#' plot(fit1, subjects=FALSE)
#' plot_fit(fit1, subjects=FALSE)
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
#' compare_means(fit1, fit2=fit2, rope=0.5)
#'
#' # compare means between two fits,
#' # use only the mu parameter of the exponentially modified gaussian distribution
#' compare_means(fit1, fit2=fit2, par="mu")
#'
#' # compare means between multiple fits
#' compare_means(fit1, fits=fit_list)
#'
#' # visualize difference in means between two fits,
#' # specify number of histogram bins and rope interval
#' plot_means_difference(fit1, fit2=fit2, bins=20, rope=0.5)
#'
#' # visualize difference in means between two fits,
#' # use only the mu parameter of the exponentially modified gaussian distribution
#' plot_means_difference(fit1, fit2=fit2, par="mu")
#'
#' # visualize difference in means between multiple fits
#' plot_means_difference(fit1, fits=fit_list)
#'
#' # visualize means of a single fit
#' plot_means(fit1)
#'
#' # visualize means of two fits
#' plot_means(fit1, fit2=fit1)
#'
#' # visualize means of two fits,
#' # use only the mu parameter of the exponentially modified gaussian distribution
#' plot_means(fit1, fit2=fit2, par="mu")
#'
#' # visualize means of multiple fits
#' plot_means(fit1, fits=fit_list)
#'
#' # draw samples from distributions underlying two fits and compare them,
#' # use a rope interval
#' compare_distributions(fit1, fit2=fit2, rope=0.5)
#'
#' # draw samples from distributions underlying multiple fits and compare them
#' compare_distributions(fit1, fits=fit_list)
#'
#' # visualize the distribution underlying a fit
#' plot_distributions(fit1)
#'
#' # visualize distributions underlying two fits
#' plot_distributions(fit1, fit2=fit2)
#'
#' # visualize distributions underlying multiple fits
#' plot_distributions(fit1, fits=fit_list)
#'
#' # visualize difference between distributions underlying two fits,
#' # use a rope interval
#' plot_distributions_difference(fit1, fit2=fit2, rope=0.05)
#'
#' # visualize difference between distributions underlying multiple fits
#' plot_distributions_difference(fit1, fits=fit_list)
#' }
#'
reaction_time_class <- setClass(
  "reaction_time_class",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)


#' @title summary
#' @description \code{summary} prints a summary of the Bayesian reaction time fit.
#' @param object reaction_time_class object.
#' @exportMethod summary
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="summary", signature(object="reaction_time_class"), definition=function(object) {
  # get means
  rt <- mean(object@extract$rt)
  mu <- mean(object@extract$mu_m)
  sigma <- mean(object@extract$mu_s)
  lambda <- mean(object@extract$mu_l)

  # HDI
  rt_hdi <- mcmc_hdi(object@extract$rt)
  mu_hdi <- mcmc_hdi(object@extract$mu_m)
  sigma_hdi <- mcmc_hdi(object@extract$mu_s)
  lambda_hdi <- mcmc_hdi(object@extract$mu_l)

  # print
  cat(sprintf("rt:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              rt, mcmcse::mcse(object@extract$rt)$se, rt_hdi[1], rt_hdi[2]))
  cat(sprintf("mu:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              mu, mcmcse::mcse(object@extract$mu_m)$se, mu_hdi[1], mu_hdi[2]))
  cat(sprintf("sigma:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              sigma, mcmcse::mcse(object@extract$mu_s)$se, sigma_hdi[1], sigma_hdi[2]))
  cat(sprintf("lambda:\t\t%.4f +/- %.5f\t95%% HDI: [%.4f, %.4f]\n",
              lambda, mcmcse::mcse(object@extract$mu_l)$se, lambda_hdi[1], lambda_hdi[2]))
})


#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian reaction time fit.
#' @param object reaction_time_class object.
#' @exportMethod show
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="show", signature(object="reaction_time_class"), definition=function(object) {
  # print
  show(object@fit)
})


#' @title plot
#' @description \code{plot} plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subjects level (subjects=TRUE) or on the group level (subjects=FALSE).
#' @param x reaction_time_class object.
#' @param y empty dummy variable, ignore this.
#' @param ... subjects - plot fits on a subject level (default = TRUE).
#' @exportMethod plot
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="plot", signature(x="reaction_time_class", y="missing"), definition=function(x, ...) {
  return(plot_fit(object=x, ...))
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subjects level (subjects=TRUE) or on the group level (subjects=FALSE).
#' @param object reaction_time_class object.
#' @param ... subjects - plot fits on a subject level (default = TRUE).
#' @rdname reaction_time_class-plot_fit
#' @aliases plot_fit_reaction_time
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="plot_fit", signature(object="reaction_time_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  rt <- x <- y <- NULL

  arguments <- list(...)

  # plot on a subject level?
  subjects <- TRUE
  if (!is.null(arguments$subjects)) {
    subjects <- arguments$subjects
  }

  df_data <- data.frame(rt=object@data$t, s=object@data$s)
  x_min <- 0

  if (!subjects) {
    mu_m <- mean(object@extract$mu_m)
    mu_s <- mean(object@extract$mu_s)
    mu_l <- mean(object@extract$mu_l)

    x_max <- max(emg::remg(1000, mu=mu_m, sigma=mu_s, lambda=mu_l), object@data$t)
    x_max <- x_max + 0.1*abs(x_max)

    step <- (x_max - x_min) / 1000
    df_fit <- data.frame(x = seq(x_min, x_max, step),
                         y = emg::demg(seq(x_min, x_max, step),
                                       mu = mu_m,
                                       sigma = mu_s,
                                       lambda = mu_l))

    graph <- ggplot(df_data, aes(x=rt)) +
      geom_density(fill="#3182bd", alpha=0.4, color=NA) +
      geom_line(data=df_fit, aes(x=x, y=y)) +
      xlab("reaction time")
  } else {
    df_fit <- NULL
    n <- length(unique(object@data$s))

    x_max <- max(object@data$t)
    x_max <- x_max + 0.1*abs(x_max)

    for (i in 1:n) {
      step <- (x_max - x_min) / 1000
      df <- data.frame(x = seq(x_min, x_max, step),
                       s = i,
                       y = emg::demg(seq(x_min, x_max, step),
                                     mu = mean(object@extract$mu[,i]),
                                     sigma = mean(object@extract$sigma[,i]),
                                     lambda = mean(object@extract$lambda[,i])))

      df_fit <- rbind(df_fit, df)
    }

    # ncol
    n_col <- ceiling(sqrt(n))

    # density per subject
    graph <- ggplot(df_data, aes(x=rt)) +
      geom_density(fill="#3182bd", alpha=0.4, color=NA) +
      geom_line(data=df_fit, aes(x=x, y=y)) +
      facet_wrap(~ s, ncol=n_col) +
      xlab("reaction time")
  }

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object reaction_time_class object.
#' @rdname reaction_time_class-plot_trace
#' @aliases plot_trace_reaction_time
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="plot_trace", signature(object="reaction_time_class"), definition=function(object) {
  rstan::traceplot(object@fit, pars=c("mu_m", "mu_s", "mu_l"), inc_warmup = TRUE)
})


#' @title get_parameters
#' @description \code{get_parameters} returns a dataframe with values of fitted parameters.
#' @param object reaction_time_class object.
#' @rdname reaction_time_class-get_parameters
#' @aliases get_parameters_reaction_time
#' @return A data frame with parameter values.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="get_parameters", signature(object="reaction_time_class"), definition=function(object) {
  df <- data.frame(rt=object@extract$rt,
                   mu=object@extract$mu_m,
                   sigma=object@extract$mu_s,
                   lambda=object@extract$mu_l)

  return(df)
})


#' @title get_subject_parameters
#' @description \code{get_subject_parameters} returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#' @param object reaction_time_class object.
#' @rdname reaction_time_class-get_subject_parameters
#' @aliases get_subject_parameters_reaction_time
#' @return A data frame with parameter values.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="get_subject_parameters", signature(object="reaction_time_class"), definition=function(object) {
  df <- data.frame(rt=numeric(), mu=numeric(), sigma=numeric(), lambda=numeric(), subject=numeric())

  n <- length(unique(object@data$s))

  for (i in 1:n) {
    df_subject <- data.frame(rt = object@extract$rt_subjects[,i],
                             mu = object@extract$mu[,i],
                             sigma = object@extract$sigma[,i],
                             lambda = object@extract$lambda[,i],
                             subject = i)

    df <- rbind(df, df_subject)
  }

  return(df)
})


#' @title compare_means
#' @description \code{compare_means} prints difference in reaction times between two groups or multiple groups.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, fits - a list of reaction_time_class objects, rope - region of practical equivalence, par - specific parameter of comparison (mu or lambda).
#' @rdname reaction_time_class-compare_means
#' @aliases compare_means_reaction_time
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="compare_means", signature(object="reaction_time_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_means function are invalid, compare_means(reaction_time_class, fit2=reaction_time_class) or compare_means(reaction_time_class, fits=list) is required! You can optionallly provide the rope parameter, e.g. compare_means(reaction_time_class, fit2=reaction_time_class, rope=numeric). You can also execute the comparison through only the mu or the lamdba parameter, e.g. compare_means(reaction_time_class, fit2=reaction_time_class, par=\"mu\")."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # compare only through one parameter
  par <- NULL
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "mu" || par == "lambda")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are mu and lambda! Using the default setting for comparison.", par)
      warning(w)
      par <- NULL
    } else {
      cat(sprintf("\n---------- Using only the %s parameter. ----------\n\n", par))
    }
  }

  # first group data
  y <- list()
  if (is.null(par)) {
    y[[1]] <- object@extract$rt
  } else if (par == "mu") {
    y[[1]] <- object@extract$mu_m
  } else if (par == "lambda") {
    y[[1]] <- object@extract$mu_l
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
      y[[2]] <- fit2@extract$rt
    } else if (par == "mu") {
      y[[2]] <- fit2@extract$mu_m
    } else if (par == "lambda") {
      y[[2]] <- fit2@extract$mu_l
    }
  } else if (!is.null(arguments$fits)) {
    # provided a list of fits
    i <- 2
    for (fit in arguments$fits) {
      if (!("reaction_time_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid reaction_time_class object.")
      }

      if (is.null(par)) {
        y[[i]] <- fit@extract$rt
      } else if (par == "mu") {
        y[[i]] <- fit@extract$mu_m
      } else if (par == "lambda") {
        y[[i]] <- fit@extract$mu_l
      }

      i <- i + 1
    }
  } else {
    stop(wrong_arguments)
  }

  n <- length(y)
  comparison_matrix <- matrix(nrow = n, ncol = n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      cat(sprintf("\n---------- Group %d vs Group %d ----------\n", i, j))
      result <- difference(y1=y[[i]], y2=y[[j]], rope=rope, group1=i, group2=j)
      comparison_matrix[j,i] <- result[1]
      comparison_matrix[i,j] <- result[2]
      cat("\n")
    }
  }

  # largest/smallest probabilities
  if (n > 2) {
    cat("-----------------------------------------")
    cat("\nProbabilities that a certain group is\nsmallest/largest or equal to all others:\n\n")
    smallest_largest <- is_smallest_or_largest(data=y, rope=rope)
    print(smallest_largest)
    cat("\n\n")
    return(list(comparison_matrix=comparison_matrix, smallest_largest=smallest_largest))
  } else {
    return(comparison_matrix)
  }
})


#' @title plot_means_difference
#' @description \code{plot_means_difference} a visualization of the difference between two groups or multiple groups.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, fits - a list of reaction_time_class objects, rope - region of practical equivalence, bins - number of bins in the histogram, par - specific parameter of comparison (mu or lambda).
#' @rdname reaction_time_class-plot_means_difference
#' @aliases plot_means_difference_reaction_time
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="plot_means_difference", signature(object="reaction_time_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_means_difference function are invalid, plot_means_difference(reaction_time_class, fit2=reaction_time_class) or plot_means_difference(reaction_time_class, fits=list) is required! You can optionallly provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_means_difference(reaction_time_class, fit2=reaction_time_class, rope=numeric, bins=numeric). You can also visualize the difference through only the mu or the lamdba parameter, e.g. plot_means_difference(reaction_time_class, fit2=reaction_time_class, par=\"mu\")."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # compare only through one parameter
  par <- NULL
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "mu" || par == "lambda")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are mu and lambda! Using the default setting for comparison.", par)
      warning(w)
      par <- NULL
    } else {
      cat(sprintf("\n---------- Using only the %s parameter. ----------\n\n", par))
    }
  }

  # first group data
  y <- list()
  if (is.null(par)) {
    y[[1]] <- object@extract$rt
  } else if (par == "mu") {
    y[[1]] <- object@extract$mu_m
  } else if (par == "lambda") {
    y[[1]] <- object@extract$mu_l
  }

  # limits
  x_max <- max(y[[1]])

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }

    if (is.null(par)) {
      y[[2]] <- fit2@extract$rt
    } else if (par == "mu") {
      y[[2]] <- fit2@extract$mu_m
    } else if (par == "lambda") {
      y[[2]] <- fit2@extract$mu_l
    }
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in arguments$fits) {
      if (!("reaction_time_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid reaction_time_class object.")
      }

      if (is.null(par)) {
        y[[i]] <- fit@extract$rt
      } else if (par == "mu") {
        y[[i]] <- fit@extract$mu_m
      } else if (par == "lambda") {
        y[[i]] <- fit@extract$mu_l
      }

      # limits
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
    graph <- plot_difference(y1=y[[1]], y2=y[[2]], rope=rope, bins=bins)
    return(graph)
  } else {
    x_max <- x_max + 0.1*x_max

    graphs <- list()
    n <- length(y)
    for (i in 1:n) {
      for (j in i:n) {
        # if both are equal plot means, else plot difference
        if (i == j) {
          df <- data.frame(value=y[[i]])
          index <- (i-1)*n + i
          graphs[[index]] <- ggplot() +
            geom_density(data=df, aes(x=value), fill="#3182bd", color=NA, alpha=0.4) +
            xlab("reaction time") +
            xlim(0, x_max)
        } else {
          index1 <- (i-1)*n + j
          graphs[[index1]] <- plot_difference(y1=y[[i]], y2=y[[j]], rope=rope, bins=bins, nrow=n)

          index2 <- (j-1)*n + i
          graphs[[index2]] <- plot_difference(y1=y[[j]], y2=y[[i]], rope=rope, bins=bins, nrow=n)
        }
      }
    }

    # cowplot
    graph <- suppressWarnings(cowplot::plot_grid(plotlist=graphs, nrow=n, ncol=n, scale=0.9))
    return(graph)
  }
})


#' @title plot_means
#' @description \code{plot_means} plots density of means for one, two or multiple groups.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, fits - a list of reaction_time_class objects, par - plot a specific parameter (mu or lambda).
#' @rdname reaction_time_class-plot_means
#' @aliases plot_means_reaction_time
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="plot_means", signature(object="reaction_time_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  group <- value <- NULL

  # extract arguments
  arguments <- list(...)

  # compare only through one parameter
  par <- NULL
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "mu" || par == "lambda")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are mu and lambda! Using the default setting for comparison.", par)
      warning(w)
      par <- NULL
    } else {
      cat(sprintf("\n---------- Using only the %s parameter. ----------\n\n", par))
    }
  }

  # first group data
  df <- NULL
  if (is.null(par)) {
    df <- data.frame(value=object@extract$rt, group="1")
  } else if (par == "mu") {
    df <- data.frame(value=object@extract$mu_m, group="1")
  } else if (par == "lambda") {
    df <- data.frame(value=object@extract$mu_l, group="1")
  }

  # second group data
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }

      if (is.null(par)) {
        df <- rbind(df, data.frame(value=fit2@extract$rt, group="2"))
      } else if (par == "mu") {
        df <- rbind(df, data.frame(value=fit2@extract$mu_m, group="2"))
      } else if (par == "lambda") {
        df <- rbind(df, data.frame(value=fit2@extract$mu_l, group="2"))
      }
    } else if (!is.null(arguments$fits)) {
      i <- 2
      for (fit in arguments$fits) {
        if (!("reaction_time_class" %in% class(fit))) {
          stop("One of the fits in the fits list is not a valid reaction_time_class object.")
        }

        if (is.null(par)) {
          df <- rbind(df, data.frame(value=fit@extract$rt, group=as.factor(i)))
        } else if (par == "mu") {
          df <- rbind(df, data.frame(value=fit@extract$mu_m, group=as.factor(i)))
        } else if (par == "lambda") {
          df <- rbind(df, data.frame(value=fit@extract$mu_l, group=as.factor(i)))
        }

        i <- i + 1
      }
    }
  }

  # limits
  x_max <- max(df$value)
  x_max <- x_max + 0.1*x_max

  # plot
  graph <- ggplot() +
    geom_density(data=df, aes(x=value, fill=group), color=NA, alpha=0.4) +
    xlab("reaction time") +
    xlim(0, x_max)

  n_groups <- max(as.numeric(df$group))
  if (n_groups == 2) {
    graph <- graph +
      scale_fill_manual(values=c("#3182bd", "#ff4e3f"))
  } else if (n_groups > 2) {
    graph <- graph +
      scale_fill_hue()
  } else {
    graph <- graph +
      scale_fill_manual(values=c("#3182bd")) +
      theme(legend.position="none")
  }

  return(suppressWarnings(graph))
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group or from samples drawn from distributions of multiple groups.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, fits - a list of reaction_time_class objects, rope - region of practical equivalence.
#' @rdname reaction_time_class-compare_distributions
#' @aliases compare_distributions_reaction_time
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="compare_distributions", signature(object="reaction_time_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(reaction_time_class, fit2=reaction_time_class) or compare_distributions(reaction_time_class, fits=list) is required! You can also provide the rope parameter, e.g. compare_distributions(reaction_time_class, fit2=reaction_time_class, rope=numeric)."

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
  y[[1]] <- emg::remg(n,
                  mu=mean(object@extract$mu_m),
                  sigma=mean(object@extract$mu_s),
                  lambda=mean(object@extract$mu_l))

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }

    y[[2]] <- emg::remg(n,
                    mu=mean(fit2@extract$mu_m),
                    sigma=mean(fit2@extract$mu_s),
                    lambda=mean(fit2@extract$mu_l))
  } else if (!is.null(arguments$fits)) {
    # provided a list of fits
    i <- 2
    for (fit in arguments$fits) {
      if (!("reaction_time_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid reaction_time_class object.")
      }

      y[[i]] <- emg::remg(n,
                          mu=mean(fit@extract$mu_m),
                          sigma=mean(fit@extract$mu_s),
                          lambda=mean(fit@extract$mu_l))

      i <- i + 1
    }
  } else {
    stop(wrong_arguments)
  }

  n <- length(y)
  comparison_matrix <- matrix(nrow = n, ncol = n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      cat(sprintf("\n---------- Group %d vs Group %d ----------\n", i, j))
      result <- difference(y1=y[[i]], y2=y[[j]], rope=rope, group1=i, group2=j)
      comparison_matrix[j,i] <- result[1]
      comparison_matrix[i,j] <- result[2]
      cat("\n")
    }
  }

  # largest/smallest probabilities
  if (n > 2) {
    cat("-----------------------------------------")
    cat("\nProbabilities that a certain group is\nsmallest/largest or equal to all others:\n\n")
    smallest_largest <- is_smallest_or_largest(data=y, rope=rope)
    print(smallest_largest)
    cat("\n\n")
    return(list(comparison_matrix=comparison_matrix, smallest_largest=smallest_largest))
  } else {
    return(comparison_matrix)
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the fitted distribution, for one, two or multiple fits.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, fits - a list of reaction_time_class objects.
#' @rdname reaction_time_class-plot_distributions
#' @aliases plot_distributions_reaction_time
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="plot_distributions", signature(object="reaction_time_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  group <- x <- y <- NULL

  # first group data
  mus <- vector()
  sigmas <- vector()
  lambdas <- vector()
  mus[[1]] <- mean(object@extract$mu_m)
  sigmas[[1]] <- mean(object@extract$mu_s)
  lambdas[[1]] <- mean(object@extract$mu_l)

  # limits
  x_max <- max(emg::remg(10000, mu=mus[[1]], sigma=sigmas[[1]], lambda=lambdas[[1]]))
  x_max <- x_max + 0.1*abs(x_max)

  # second group data
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      mus[[2]] <- mean(fit2@extract$mu_m)
      sigmas[[2]] <- mean(fit2@extract$mu_s)
      lambdas[[2]] <- mean(fit2@extract$mu_l)

      x_max2 <- max(emg::remg(10000, mu=mus[[2]], sigma=sigmas[[2]], lambda=lambdas[[2]]))
      x_max2 <- x_max2 + 0.1*abs(x_max2)
      x_max <- max(x_max, x_max2)
    } else if (!is.null(arguments$fits)) {
      i <- 2
      for (fit in arguments$fits) {
        if (!("reaction_time_class" %in% class(fit))) {
          stop("One of the fits in the fits list is not a valid reaction_time_class object.")
        }

        mus[[i]] <- mean(fit@extract$mu_m)
        sigmas[[i]] <- mean(fit@extract$mu_s)
        lambdas[[i]] <- mean(fit@extract$mu_l)

        x_max2 <- max(emg::remg(10000, mu=mus[[2]], sigma=sigmas[[2]], lambda=lambdas[[2]]))
        x_max2 <- x_max2 + 0.1*abs(x_max2)
        x_max <- max(x_max, x_max2)

        i <- i + 1
      }
    }
  }

  # calculate data points
  step <- 1 / 1000
  df <- data.frame(x=numeric(), y=numeric(), group=factor())
  n_groups <- length(mus)
  for (i in 1:n_groups) {
    df_group <- data.frame(x = seq(0, x_max, step),
                           y = emg::demg(seq(0, x_max, step),
                                         mu = mus[i],
                                         sigma = sigmas[i],
                                         lambda = lambdas[i]),
                           group=as.factor(i))

    df <- rbind(df, df_group)
  }

  # plot
  graph <- ggplot() +
    geom_area(data=df, aes(x=x, y=y, fill=group), alpha=0.4, position="identity") +
    xlab("reaction time") +
    ylab("density")

  if (n_groups == 2) {
    graph <- graph +
      scale_fill_manual(values=c("#3182bd", "#ff4e3f"))
  } else if (n_groups > 2) {
    graph <- graph +
      scale_fill_hue()
  } else {
    graph <- graph +
      scale_fill_manual(values=c("#3182bd")) +
      theme(legend.position="none")
  }

  return(suppressWarnings(graph))
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the second group or between multiple groups.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, fits - a list of reaction_time_class objects, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname reaction_time_class-plot_distributions_difference
#' @aliases plot_distributions_difference_reaction_time
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?reaction_time_class
#'
setMethod(f="plot_distributions_difference", signature(object="reaction_time_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(reaction_time_class, fit2=reaction_time_class) or plot_distributions_difference(reaction_time_class, fits=list) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_distributions_difference(reaction_time_class, fit2=reaction_time_class, rope=numeric, bins=numeric)."

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
  y[[1]] <- emg::remg(n,
                  mu=mean(object@extract$mu_m),
                  sigma=mean(object@extract$mu_s),
                  lambda=mean(object@extract$mu_l))

  # limits
  x_max <- max(y[[1]])

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "reaction_time_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y[[2]] <- emg::remg(n,
                        mu=mean(fit2@extract$mu_m),
                        sigma=mean(fit2@extract$mu_s),
                        lambda=mean(fit2@extract$mu_l))
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in arguments$fits) {
      if (!("reaction_time_class" %in% class(fit))) {
        stop("One of the fits in the fits list is not a valid reaction_time_class object.")
      }

      y[[i]] <- emg::remg(n,
                          mu=mean(fit@extract$mu_m),
                          sigma=mean(fit@extract$mu_s),
                          lambda=mean(fit@extract$mu_l))

      # limits
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
    graph <- plot_difference(y1=y[[1]], y2=y[[2]], rope=rope, bins=bins)
    return(graph)
  } else {
    x_max <- x_max + 0.1*x_max

    graphs <- list()
    n <- length(y)
    for (i in 1:n) {
      for (j in i:n) {
        # if both are equal plot samples, else plot difference
        if (i == j) {
          df <- data.frame(value=y[[i]])
          index <- (i-1)*n + i
          graphs[[index]] <- ggplot() +
            geom_density(data=df, aes(x=value), fill="#3182bd", color=NA, alpha=0.4) +
            xlab("reaction time") +
            xlim(0, x_max)
        } else {
          index1 <- (i-1)*n + j
          graphs[[index1]] <- plot_difference(y1=y[[i]], y2=y[[j]], rope=rope, bins=bins, nrow=n)

          index2 <- (j-1)*n + i
          graphs[[index2]] <- plot_difference(y1=y[[j]], y2=y[[i]], rope=rope, bins=bins, nrow=n)
        }
      }
    }

    # cowplot
    graph <- suppressWarnings(cowplot::plot_grid(plotlist=graphs, nrow=n, ncol=n, scale=0.9))
    return(graph)
  }
})
