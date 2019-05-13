#' @title reaction_time_class
#' @import ggplot2
#' @description An S4 class for storing results of reaction time Bayesian model.
#'
#' \strong{Functions}
#'
#' summary(`reaction_time_class`): prints summary od the fit.
#'
#' print(`reaction_time_class`): prints a more detailed summary of the fit
#'
#' show(`reaction_time_class`): prints a more detailed summary of the fit.
#'
#' get_samples(`reaction_time_class`): returns a dataframe with values of fitted parameters.
#'
#' get_subject_samples(`reaction_time_class`): returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#'
#' compare(`reaction_time_class`, fit2=`reaction_time_class`): prints difference in reaction times between two groups. You can also provide the rope parameter or execute the comparison only through a chosen parameter - mu or lambda.
#'
#' plot_difference(`reaction_time_class`, fit2=`reaction_time_class`): a visualization of the difference between two groups. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through a chosen parameter - mu or lambda.
#'
#' plot_samples(`reaction_time_class`): plots density of the samples samples. You can also visualize the density only for a chosen parameter - mu or lambda.
#'
#' plot_samples(`reaction_time_class`, fit2=`reaction_time_class`): plots density for the first and the second group samples. You can also visualize the density only for a chosen parameter - mu or lambda.
#'
#' compare_distributions(`reaction_time_class`, fit2=`reaction_time_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter.
#'
#' plot_distributions(`reaction_time_class`): a visualization of the fitted distribution.
#'
#' plot_distributions(`reaction_time_class`, fit2=`reaction_time_class`): a visualization of the distribution for two fits.
#'
#' plot_distributions_difference(`reaction_time_class`, fit2=`reaction_time_class`): a visualization of the difference between the distribution of the first group and the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_fit(`reaction_time_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_trace(`reaction_time_class`): traceplot for main fitted model parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
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
#' @description \code{summary} prints summary of the Bayesian reaction time fit.
#' @param object reaction_time_class object.
#' @exportMethod summary
setMethod(f="summary", signature(object="reaction_time_class"), definition=function(object) {
  # get means
  rt <- mean(object@extract$rt)
  mu <- mean(object@extract$mu_m)
  sigma <- mean(object@extract$mu_s)
  lambda <- mean(object@extract$mu_l)

  # hdi
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
setMethod(f="show", signature(object="reaction_time_class"), definition=function(object) {
  # print
  show(object@fit)
})


#' @title get_samples
#' @description \code{get_samples} returns a dataframe with values of fitted parameters.
#' @param object reaction_time_class object.
#' @rdname reaction_time_class-get_samples
#' @aliases get_samples_reaction_time
setMethod(f="get_samples", signature(object="reaction_time_class"), definition=function(object) {
  df <- data.frame(rt=object@extract$rt,
                   mu=object@extract$mu_m,
                   sigma=object@extract$mu_s,
                   lambda=object@extract$mu_l)

  return(df)
})


#' @title get_subject_samples
#' @description \code{get_subject_samples} returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#' @param object reaction_time_class object.
#' @rdname reaction_time_class-get_subject_samples
#' @aliases get_subject_samples_reaction_time
setMethod(f="get_subject_samples", signature(object="reaction_time_class"), definition=function(object) {
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


#' @title compare
#' @description \code{compare} prints difference in reaction times between two groups.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, rope - region of practical equivalence, par - specific parameter of comparison - mu or lambda.
#' @rdname reaction_time_class-compare
#' @aliases compare_reaction_time
setMethod(f="compare", signature(object="reaction_time_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare function are invalid, compare(reaction_time_class, fit2=reaction_time_class) is required! You can optionallly provide the rope parameter, e.g. compare(reaction_time_class, fit2=reaction_time_class, rope=numeric). You can also execute the comparison through only the mu or the lamdba parameter, e.g. compare(reaction_time_class, fit2=reaction_time_class, par=\"mu\")."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
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
      cat(sprintf("Using only the %s parameter.\n", par))
    }
  }

  # first group data
  if (is.null(par)) {
    y1 <- object@extract$rt
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
      y2 <- fit2@extract$rt
    } else if (par == "mu") {
      y2 <- fit2@extract$mu_m
    } else if (par == "lambda") {
      y2 <- fit2@extract$mu_l
    }

    shared_difference(y1=y1, y2=y2, rope=rope)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_difference
#' @description \code{plot_difference} a visualization of the difference between two groups.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, rope - region of practical equivalence, bins - number of bins in the histogram, par - specific parameter of comparison - mu or lambda.
#' @rdname reaction_time_class-plot_difference
#' @aliases plot_difference_reaction_time
setMethod(f="plot_difference", signature(object="reaction_time_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_difference function are invalid, plot_difference(reaction_time_class, fit2=reaction_time_class) is required! You can optionallly provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_difference(reaction_time_class, fit2=reaction_time_class, rope=numeric, bins=numeric). You can also visualize the difference through only the mu or the lamdba parameter, e.g. plot_difference(reaction_time_class, fit2=reaction_time_class, par=\"mu\")."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
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
      cat(sprintf("Using only the %s parameter.\n", par))
    }
  }

  # first group data
  if (is.null(par)) {
    y1 <- object@extract$rt
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
      y2 <- fit2@extract$rt
    } else if (par == "mu") {
      y2 <- fit2@extract$mu_m
    } else if (par == "lambda") {
      y2 <- fit2@extract$mu_l
    }

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)
    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_samples
#' @description \code{plot_samples} plots density of the samples, or the first and the second group samples.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, par - specific parameter of comparison - mu or lambda.
#' @rdname reaction_time_class-plot_samples
#' @aliases plot_samples_reaction_time
setMethod(f="plot_samples", signature(object="reaction_time_class"), definition=function(object, ...) {
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
      cat(sprintf("Using only the %s parameter.\n", par))
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
  df2 <- NULL
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
    }
  }

  # limits
  x_min <- min(df$value)
  x_max <- max(df$value)
  diff <- x_max - x_min
  x_min <- x_min - 0.1*diff
  x_max <- x_max + 0.1*diff

  # plot
  graph <- ggplot() +
    geom_density(data=df, aes(x=value, fill=group), alpha=0.4, color=NA) +
    scale_fill_manual(values=c("#3182bd", "#ff4e3f")) +
    xlab("value") +
    xlim(x_min, x_max) +
    theme(legend.position="none")

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, rope - region of practical equivalence.
#' @rdname reaction_time_class-compare_distributions
#' @aliases compare_distributions_reaction_time
setMethod(f="compare_distributions", signature(object="reaction_time_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(reaction_time_class, fit2=reaction_time_class) is required! You can also provide the rope parameter, e.g. compare_distributions(reaction_time_class, fit2=reaction_time_class, rope=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  n <- 100000

  # first group data
  mu_m1 <- mean(object@extract$mu_m)
  mu_s1 <- mean(object@extract$mu_s)
  mu_l1 <- mean(object@extract$mu_l)
  y1 <- emg::remg(n, mu=mu_m1, sigma=mu_s1, lambda=mu_l1)

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
    y2 <- emg::remg(n, mu=mu_m2, sigma=mu_s2, lambda=mu_l2)

    shared_difference(y1=y1, y2=y2, rope=rope)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the fitted distribution, for one or two fits.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object.
#' @rdname reaction_time_class-plot_distributions
#' @aliases plot_distributions_reaction_time
setMethod(f="plot_distributions", signature(object="reaction_time_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  n <- 10000

  # first group data
  mu_m1 <- mean(object@extract$mu_m)
  mu_s1 <- mean(object@extract$mu_s)
  mu_l1 <- mean(object@extract$mu_l)

  # limits
  x_max <- max(emg::remg(1000, mu=mu_m1, sigma=mu_s1, lambda=mu_l1))
  x_max <- x_max + 0.1*x_max

  # second group data
  group2_plot <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
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

      x_max2 <- max(emg::remg(1000, mu=mu_m2, sigma=mu_s2, lambda=mu_l2))
      x_max2 <- x_max2 + 0.1*x_max2
      x_max <- max(x_max, x_max2)

      group2_plot <- stat_function(fun=emg::demg, n=n, args=list(mu=mu_m2, sigma=mu_s2, lambda=mu_l2), geom="area", fill="#ff4e3f", alpha=0.4)
    }
  }

  x_max <- ceiling(x_max)
  df_x <- data.frame(value=c(0, x_max))

  # plot
  graph <- ggplot(data=df_x, aes(x=value)) +
    stat_function(fun=emg::demg, n=n, args=list(mu=mu_m1, sigma=mu_s1, lambda=mu_l1), geom="area", fill="#3182bd", alpha=0.4) +
    group2_plot +
    xlab("value")

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the second group.
#' @param object reaction_time_class object.
#' @param ... fit2 - a second reaction_time_class object, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname reaction_time_class-plot_distributions_difference
#' @aliases plot_distributions_difference_reaction_time
setMethod(f="plot_distributions_difference", signature(object="reaction_time_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(reaction_time_class, fit2=reaction_time_class) is required! You can also provide the rope and bins (number of bins in the histogram) parameter, e.g. plot_distributions_difference(reaction_time_class, fit2=reaction_time_class, rope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    warning(wrong_arguments)
    return()
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  n <- 100000

  # first group data
  mu_m1 <- mean(object@extract$mu_m)
  mu_s1 <- mean(object@extract$mu_s)
  mu_l1 <- mean(object@extract$mu_l)
  y1 <- emg::remg(n, mu=mu_m1, sigma=mu_s1, lambda=mu_l1)

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
    y2 <- emg::remg(n, mu=mu_m2, sigma=mu_s2, lambda=mu_l2)

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)
    return(graph)
  } else {
    warning(wrong_arguments)
    return()
  }
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object reaction_time_class object.
#' @rdname reaction_time_class-plot_fit
#' @aliases plot_fit_reaction_time
setMethod(f="plot_fit", signature(object="reaction_time_class"), definition=function(object) {
  # init local varibales for CRAN check
  rt <- x <- y <- NULL

  df_data <- data.frame(rt=object@data$t, s=object@data$s)

  df_fit <- NULL
  n <- length(unique(object@data$s))

  x_min <- 0
  x_max <- max(object@data$t)
  x_max <- x_max + 0.1*x_max

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

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object reaction_time_class object.
#' @rdname reaction_time_class-plot_trace
#' @aliases plot_trace_reaction_time
setMethod(f="plot_trace", signature(object="reaction_time_class"), definition=function(object) {
  rstan::traceplot(object@fit, pars=c("mu_m", "mu_s", "mu_l"), inc_warmup = TRUE)
})
