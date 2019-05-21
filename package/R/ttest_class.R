#' @title ttest_class
#' @import ggplot2
#' @description An S4 class for storing results of Bayesian t-test results.
#'
#' \strong{Functions}
#'
#' summary(`ttest_class`): prints summary of the fit.
#'
#' print(`ttest_class`): prints a more detailed summary of the fit
#'
#' show(`ttest_class`): prints a more detailed summary of the fit.
#'
#' get_samples(`ttest_class`): returns a dataframe with values of fitted parameters.
#'
#' compare_samples(`ttest_class`, fit2=`ttest_class`): prints difference/equality of the first group against the second group. You can also provide the rope parameter.
#'
#' compare_samples(`ttest_class`, mu=`numeric`): prints difference/equality of the first group against a mean value. You can also provide the rope parameter.
#'
#' compare_samples(`ttest_class`, mu=`numeric`, sigma=`numeric`): prints difference/equality of the first group against a normal distribution provided with mean value and standard deviation. Note here that sigma is use only in the Cohens d calculation. You can also provide the rope parameter.
#'
#' compare_samples(`ttest_class`, fits=`list`): prints difference/equality of the first group and multiple other groups.
#'
#' plot_samples_difference(`ttest_class`, fit2=`ttest_class`): a visualization of the difference between the first group and the second group. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_samples_difference(`ttest_class`, mu=`numeric`): a visualization of the difference between the first group and a constant value or a normal distribution with mean value mu. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_samples_difference(`ttest_class`, fits=`list`): a visualization of the difference between multiple groups. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_samples(`ttest_class`): plots density of the samples.
#'
#' plot_samples(`ttest_class`, fit2=`ttest_class`): plots density for the first and the second group samples.
#'
#' plot_samples(`ttest_class`, mu=`numeric`): plots density for the first group samples and a mean value in case second group is defined as a normal distribution or as a constant.
#'
#' plot_samples(`ttest_class`, fits=`list`): plots density for the first group samples and samples for multiple other groups.
#'
#' compare_distributions(`ttest_class`, fit2=`ttest_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, mu=`numeric`): draws samples from distribution of the first group and compares them against a mean value. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, mu=`numeric`, sigma=`numeric`): draws samples from distribution of the first group and compares them against samples from a normal distribution with a defined mean value and variance. You can also provide the rope parameter.
#'
#' compare_distributions(`ttest_class`, fits=`list`): draws samples from distribution of the first group and compares them against samples drawn from multiple other groups.
#'
#' plot_distributions(`ttest_class`): a visualization of the fitted.
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
#' plot_fit(`ttest_class`): plots fitted model against the data. Use this function to explore the quality of your fit.
#'
#' plot_trace(`ttest_class`): traceplot for main fitted model parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Raw data for the tested group.
ttest_class <- setClass(
  "ttest_class",
  slots = c(
    extract  = "list",
    fit = "stanfit",
    data = "numeric"
  ),
  contains = "b_results"
)

#' @title summary
#' @description \code{summary} prints summary of the Bayesian ttest fit.
#' @param object ttest_class object.
#' @exportMethod summary
setMethod(f="summary", signature(object="ttest_class"), definition=function(object) {
  # get means
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)
  nu <- mean(object@extract$nu)

  # hdi
  mu_hdi <- mcmc_hdi(object@extract$mu)
  sigma_hdi <- mcmc_hdi(object@extract$sigma)
  nu_hdi <- mcmc_hdi(object@extract$nu)

  # print)
  cat(sprintf("mu:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              mu, mcmcse::mcse(object@extract$mu)$se, mu_hdi[1], mu_hdi[2]))
  cat(sprintf("sigma:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              sigma, mcmcse::mcse(object@extract$sigma)$se, sigma_hdi[1], sigma_hdi[2]))
  cat(sprintf("nu:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n", nu,
              mcmcse::mcse(object@extract$nu)$se, nu_hdi[1], nu_hdi[2]))
})


#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian ttest fit.
#' @param object ttest_class object.
#' @exportMethod show
setMethod(f="show", signature(object="ttest_class"), definition=function(object) {
  # print
  show(object@fit)
})


#' @title get_samples
#' @description \code{get_samples} returns a dataframe with values of fitted parameters.
#' @param object ttest_class object.
#' @rdname ttest_class-get_samples
#' @aliases get_samples_ttest_class
setMethod(f="get_samples", signature(object="ttest_class"), definition=function(object) {
  df <- data.frame(mu=object@extract$mu,
                   sigma=object@extract$sigma,
                   nu=object@extract$nu)

  return(df)
})


#' @title compare_samples
#' @description \code{compare_samples} prints difference/equality of the first group against the second group, against a mean value or against a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, sigma - standard deviation, fits - a list of ttest_class objects, rope - region of practical equivalence.
#' @rdname ttest_class-compare_samples
#' @aliases compare_samples_ttest
setMethod(f="compare_samples", signature(object="ttest_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_samples function are invalid, compare_samples(ttest_class, fit2=ttest_class), compare_samples(ttest_class, mu=numeric), compare_samples(ttest_class, mu=numeric, sigma=numeric) or compare_samples(ttest_class, fits=list) is required! You can also provide the rope parameter, e.g. compare_samples(ttest_class, fit2=ttest_class, rope=numeric)."

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

  # data
  y <- list()

  # first group data
  y[[1]] <- object@extract$mu
  sigma1 <- mean(object@extract$sigma)
  n <- length(y1)

  # second group data
  sigma2 <- NULL
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    y[[2]] <- fit2@extract$mu
    sigma2 <- mean(fit2@extract$sigma)
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    y[[2]] <- arguments$mu;

    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
    }
  } else if (!is.null(arguments$fits)) {
    # provided a list of fits
    i <- 2
    for (fit in arguments$fits) {
      if (class(fit) != "ttest_class") {
        warning("One of the fits in the fits list is not a valid ttest_class object.")
        return()
      }
      y[[i]] <- fit@extract$mu
      i <- i + 1
    }
  } else {
    warning(wrong_arguments)
    return()
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
    cat("\nProbabilities that a certain group is smallest/largest or equal to all others:\n")
    print(is_smallest_or_largest(data=y, rope=rope))
  }

  # add Cohen's d if 2 groups are provided
  if (!is.null(sigma2)) {
    diff <- mean(y1) - mean(y2)
    cohens_d <- diff / sqrt((n*sigma1^2 + n*sigma2^2) / (n + n - 2));
    cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
  }

  return(comparison_matrix)
})


#' @title plot_samples_difference
#' @description \code{plot_samples_difference} a visualization of the difference of the first group against the second group, against multiple groups, against a mean value or against a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, fits - a list of ttest_class objects, mu - mean value, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname ttest_class-plot_samples_difference
#' @aliases plot_samples_difference_ttest
setMethod(f="plot_samples_difference", signature(object="ttest_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_samples_difference function are invalid, plot_samples_difference(ttest_class, fit2=ttest_class), plot_samples_difference(ttest_class, fits=list) or plot_samples_difference(ttest_class, mu=numeric) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_samples_difference(ttest_class, fit2=ttest_class, rope=numeric, bins=numeric)."

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

  # first group data
  y <- list()
  y[[1]] <- object@extract$mu

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
    y[[2]] <- fit2@extract$mu
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    y[[2]] <- arguments$mu;
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in fits) {
      y[[i]] <- fit@extract$mu

      # limits
      x_min <- min(x_min, y[[i]])
      x_max <- max(x_max, y[[i]])

      i <- i + 1
    }
  } else {
    warning(wrong_arguments)
    return()
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
    diff <- x_max - x_min
    x_min <- x_min - 0.1*diff
    x_max <- x_max + 0.1*diff

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
            xlab("value") +
            xlim(x_min, x_max)
        } else {
          index1 <- (i-1)*n + j
          graphs[[index1]] <- plot_difference(y1=y[[i]], y2=y[[j]], rope=rope, bins=bins, nrow=n)

          index2 <- (j-1)*n + i
          graphs[[index2]] <- plot_difference(y1=y[[j]], y2=y[[i]], rope=rope, bins=bins, nrow=n)
        }
      }
    }

    # cowplot
    graph <- cowplot::plot_grid(plotlist=graphs, nrow=n, ncol=n, scale=0.9)
    return(graph)
  }
})


#' @title plot_samples
#' @description \code{plot_samples} plots density of the samples, the first and the second group samples, samples of multiple groups or a mean value in case second group is defined as a constant.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, mu - mean value, fits - a list of ttest_class objects.
#' @rdname ttest_class-plot_samples
#' @aliases plot_samples_ttest
setMethod(f="plot_samples", signature(object="ttest_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  # first group data
  n_groups <- 1
  mu <- object@extract$mu
  df <- data.frame(value=mu, group=as.factor(1))

  # limits
  x_min <- min(mu)
  x_max <- max(mu)

  # second group data
  mu2 <- NULL
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "ttest_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      n_groups = 2
      mu <- fit2@extract$mu
      df <- rbind(df, data.frame(value=mu, group=as.factor(2)))

      # limits
      x_min <- min(x_min, mu)
      x_max <- max(x_max, mu)
    } else if (!is.null(arguments$mu)) {
      # provided mu and sigma
      mu2 <- arguments$mu;
    } else if (!is.null(arguments$fits)) {
      i <- 2
      for (fit in arguments$fits) {
        mu <- fit@extract$mu
        df <- rbind(df, data.frame(value=mu, group=as.factor(i)))
        n_groups <- i
        i <- i + 1

        # limits
        x_min <- min(x_min, mu)
        x_max <- max(x_max, mu)
      }
    }
  }

  # plot
  graph <- ggplot() +
    geom_density(data=df, aes(x=value, fill=group), color=NA, alpha=0.4) +
    xlab("value")

  if (n_groups == 2) {
    graph <- graph +
      scale_fill_manual(values=c("#3182bd", "#ff4e3f"))
  } else if (n_groups > 2) {
    graph <- graph +
      scale_fill_hue()
  } else if (n_groups == 1 & !is.null(mu2)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    # limits
    x_min <- min(x_min, mu2)
    x_max <- max(x_max, mu2)

    graph <- graph +
      geom_segment(aes(x=mu2, xend=mu2, y=0, yend=y_max[2]*1.05), size=1.5, color="#ff4e3f", alpha=0.4) +
      geom_text(aes(label=sprintf("%.2f", mu2), x=mu2, y=y_max[2]*1.08), size=4) +
      scale_fill_manual(values=c("#3182bd")) +
      theme(legend.position="none")
  } else {
    graph <- graph +
      scale_fill_manual(values=c("#3182bd"))
  }

  # limits
  diff <- x_max - x_min

  x_min <- x_min - 0.1*diff
  x_max <- x_max + 0.1*diff

  graph <- graph + xlim(x_min, x_max)

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group, against samples drawn from multiple groups, against a mean value or against samples from a normal distribution with a defined mean value and variance.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, fits - a list of ttest_class objects, mu - mean value, sigma - standard deviation, rope - region of practical equivalence.
#' @rdname ttest_class-compare_distributions
#' @aliases compare_distributions_ttest
setMethod(f="compare_distributions", signature(object="ttest_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(ttest_class, fit2=ttest_class), compare_distributions(ttest_class, fits=list), compare_distributions(ttest_class, mu=numeric) or compare_distributions(ttest_class, mu=numeric, sigma=numeric) is required! You can also provide the rope parameter, e.g. compare_distributions(ttest_class, fit2=ttest_class, rope=numeric)."

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

  # first group data
  n <- 100000
  sigma1 <- mean(object@extract$sigma)
  y[[1]] <- metRology::rt.scaled(n,
                                 df=mean(object@extract$nu),
                                 mean=mean(object@extract$mu),
                                 sd=sigma1)

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
                                   df=mean(fit2@extract$nu),
                                   mean=mean(fit2@extract$mu),
                                   sd=sigma2)
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma;
      y[[2]] <- stats::rnorm(n, arguments$mu, sigma2)
    } else {
      y[[2]] <- stats::rnorm(n, arguments$mu, 0)
    }
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in fits) {
      if (class(fit) != "ttest_class") {
        warning("One of the fits in the fits list is not a valid ttest_class object.")
        return()
      }

      y[[i]] <- metRology::rt.scaled(n,
                                     df=mean(fit@extract$nu),
                                     mean=mean(fit@extract$mu),
                                     sd=mean(fit@extract$sigma))

      i <- i + 1
    }
  } else {
    warning(wrong_arguments)
    return()
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
    cat("\nProbabilities that a certain group is smallest/largest or equal to all others:\n")
    print(is_smallest_or_largest(data=y, rope=rope))
  }

  # add Cohen's d if 2 groups are provided
  if (!is.null(sigma2)) {
    diff <- mean(y1) - mean(y2)
    cohens_d <- diff / sqrt((n*sigma1^2 + n*sigma2^2) / (n + n - 2));
    cat(sprintf("\nCohen's d: %.2f\n", cohens_d))
  }

  return(comparison_matrix)
})


#' @title plot_distributions
#' @description \code{plot_distributions} visualizes distributions underlying tested groups.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, fits - a list of ttest_class objects, mu - mean value, sigma - standard deviation.
#' @rdname ttest_class-plot_distributions
#' @aliases plot_distributions_ttest
setMethod(f="plot_distributions", signature(object="ttest_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  # first group data
  n_groups <- 1
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

      n_groups <- 2
    } else if (!is.null(arguments$mu)) {
      # provided mu and/or sigma
      mu2 <- arguments$mu;

      if (!is.null(arguments$sigma)) {
        sigma2 <- arguments$sigma;
      }
    } else if (!is.null(arguments$fits)) {
      i <- 2
      for (fit in arguments$fits) {
        nus[i] <- mean(fit@extract$nu)
        mus[i] <- mean(fit@extract$mu)
        sigmas[i] <- mean(fit@extract$sigma)
        n_groups <- i
        i <- i + 1
      }
    }
  }

  # get boundaries
  x_min <- min(mus - 4*sigmas)
  x_max <- max(mus + 4*sigmas)

  if (!is.null(mu2) & !is.null(sigma2)) {
    x_min <- min(x_min, mu2 - 4*sigma2)
    x_max <- max(x_max, mu2 + 4*sigma2)
  } else if (!is.null(mu2)) {
    if (mu2 < x_min) {
      x_min <- mu2
      x_min <- x_min - (0.1*(x_max-x_min))
    } else if (mu2 > x_max) {
      x_max <- mu2
      x_max <- x_max + (0.1*(x_max-x_min))
    }
  }

  # calculate data points
  step <- (x_max - x_min) / 10000
  df <- data.frame(x=numeric(), y=numeric(), group=factor())
  for (i in 1:n_groups) {
    df_group <- data.frame(x = seq(x_min, x_max, step),
                           y = metRology::dt.scaled(seq(x_min, x_max, step),
                                                   df = nus[i],
                                                   mean = mus[i],
                                                   sd = sigmas[i]),
                           group=as.factor(i))

    df <- rbind(df, df_group)
  }

  if (!is.null(mu2) & !is.null(sigma2)) {
    df_group <- data.frame(x = seq(x_min, x_max, step),
                           y = stats::dnorm(seq(x_min, x_max, step),
                                                    mean = mu2,
                                                    sd = sigma2),
                           group=as.factor(2))

    df <- rbind(df, df_group)

    n_groups <- 2
  }

  # plot
  graph <- ggplot() +
    geom_area(data=df, aes(x=x, y=y, fill=group), alpha=0.4) +
    xlab("value")

  if (n_groups == 2) {
    graph <- graph +
      scale_fill_manual(values=c("#3182bd", "#ff4e3f"))
  } else if (n_groups > 2) {
    graph <- graph +
      scale_fill_hue()
  } else if (n_groups == 1 & !is.null(mu2)) {
    y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

    graph <- graph +
      geom_segment(aes(x=mu2, xend=mu2, y=0, yend=y_max[2]*1.05), size=1.5, color="#ff4e3f", alpha=0.4) +
      geom_text(aes(label=sprintf("%.2f", mu2), x=mu2, y=y_max[2]*1.08), size=4) +
      scale_fill_manual(values=c("#3182bd")) +
      theme(legend.position="none")
  }

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group, the distribution or a constant value for the second group or between multiple distributions.
#' @param object ttest_class object.
#' @param ... fit2 - a second ttest_class object, fits - a list of ttest_class objects, mu - mean value, sigma - standard deviation, rope - region of practical equivalence, bins - number of bins in the histogram.
#' @rdname ttest_class-plot_distributions_difference
#' @aliases plot_distributions_difference_ttest
setMethod(f="plot_distributions_difference", signature(object="ttest_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(ttest_class, fit2=ttest_class), plot_distributions_difference(ttest_class, mu=numeric), plot_distributions_difference(ttest_class, mu=numeric, sigma=numeric) or plot_distributions_difference(ttest_class, fits=list) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_distributions_difference(ttest_class, fit2=ttest_class, rope=numeric, bins=numeric)."

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

  # first group data
  y <- list()
  n <- 100000
  y[[1]] <- metRology::rt.scaled(n,
                                 df=mean(object@extract$nu),
                                 mean=mean(object@extract$mu),
                                 sd=mean(object@extract$sigma))

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
                                   df=mean(fit2@extract$nu),
                                   mean=mean(fit2@extract$mu),
                                   sd=mean(fit2@extract$sigma))
  } else if (!is.null(arguments$mu)) {
    # provided mu and sigma
    sigma2 <- 0

    if (!is.null(arguments$sigma)) {
      sigma2 <- arguments$sigma
    }

    y[[2]] <- stats::rnorm(n, arguments$mu, sigma2)
  } else if (!is.null(arguments$fits)) {
    i <- 2
    for (fit in fits) {
      y[[i]] <- metRology::rt.scaled(n,
                                     df=mean(fit@extract$nu),
                                     mean=mean(fit@extract$mu),
                                     sd=mean(fit@extract$sigma))

      # limits
      x_min <- min(x_min, y[[i]])
      x_max <- max(x_max, y[[i]])

      i <- i + 1
    }
  } else {
    warning(wrong_arguments)
    return()
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
    diff <- x_max - x_min
    x_min <- x_min - 0.1*diff
    x_max <- x_max + 0.1*diff

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
            xlab("value") +
            xlim(x_min, x_max)
        } else {
          index1 <- (i-1)*n + j
          graphs[[index1]] <- plot_difference(y1=y[[i]], y2=y[[j]], rope=rope, bins=bins, nrow=n)

          index2 <- (j-1)*n + i
          graphs[[index2]] <- plot_difference(y1=y[[j]], y2=y[[i]], rope=rope, bins=bins, nrow=n)
        }
      }
    }

    # cowplot
    graph <- cowplot::plot_grid(plotlist=graphs, nrow=n, ncol=n, scale=0.9)
    return(graph)
  }
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit.
#' @param object ttest_class object.
#' @rdname ttest_class-plot_fit
#' @aliases plot_fit_ttest
setMethod(f="plot_fit", signature(object="ttest_class"), definition=function(object) {
  # init local varibales for CRAN check
  value <- NULL

  n <- 10000
  df_data <- data.frame(value=object@data)

  nu <- mean(object@extract$nu)
  mu <- mean(object@extract$mu)
  sigma <- mean(object@extract$sigma)

  # get x range
  x_min <- min(mu - 4*sigma, df_data$value)
  x_max <- max(mu + 4*sigma, df_data$value)

  diff <- x_max - x_min
  x_min <- x_min - 0.1*diff
  x_max <- x_max + 0.1*diff

  df_x <- data.frame(x=c(x_min, x_max))

  graph <- ggplot(data=df_x) +
    geom_density(data=df_data, aes(x=value), fill="#3182bd", alpha=0.4, color=NA) +
    stat_function(fun=metRology::dt.scaled, n=n, args=list(df=nu, mean=mu, sd=sigma), colour="#3182bd", size=1) +
    xlab("value") +
    xlim(x_min, x_max)

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object ttest_class object.
#' @rdname ttest_class-plot_trace
#' @aliases plot_trace_ttest
setMethod(f="plot_trace", signature(object="ttest_class"), definition=function(object) {
  traceplot(object@fit, pars=c("mu", "sigma"), inc_warmup=TRUE)
})
