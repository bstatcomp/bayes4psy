#' @title linear_class
#' @import dplyr ggplot2
#' @description An S4 class for storing results of normal linear model.
#'
#' \strong{Functions}
#'
#' summary(`linear_class`): prints a summary of the fit.
#'
#' print(`linear_class`): prints a more detailed summary of the fit
#'
#' show(`linear_class`): prints a more detailed summary of the fit.
#'
#' plot(`linear_class`): plots fitted model against the data. Use this function to explore the quality of your fit. Fit will be plotted on the subject level.
#'
#' plot(`linear_class`, subjects='boolean'): plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subject level (subjects=TRUE) or on the subjects level (subjects=FALSE).
#'
#' plot_fit(`linear_class`): plots fitted model against the data. Use this function to explore the quality of your fit. Fit will be plotted on the subject level.
#'
#' plot_fit(`linear_class`, subjects='boolean'): plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subject level (subjects=TRUE) or on the subjects level (subjects=FALSE).
#'
#' plot_trace(`linear_class`): traceplot for main fitted model parameters.
#'
#' get_parameters(`linear_class`): returns a dataframe with values of fitted parameters.
#'
#' get_subject_parameters(`linear_class`): returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#'
#' compare_means(`linear_class`, fit2=`linear_class`): prints difference in slope and intercept between two groups. You can also provide the rope parameter.
#'
#' plot_means_difference(`linear_class`, fit2=`linear_class`): a visualization of the difference between two groups. You can plot only slope or intercept by using the par parameter. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' plot_means(`linear_class`): plots density of means. You can plot only slope or intercept by using the par parameter.
#'
#' plot_means(`linear_class`, fit2=`linear_class`): plots density for the first and the second group means. You can plot only slope or intercept by using the par parameter.
#'
#' compare_distributions(`linear_class`, fit2=`linear_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#'
#' plot_distributions(`linear_class`): a visualization of the fitted distribution.
#'
#' plot_distributions(`linear_class`, fit2=`linear_class`): a visualization of two fitted distribution.
#'
#' plot_distributions_difference(`linear_class`, fit2=`linear_class`): a visualization of the difference between the distribution of the first group and the second group. You can plot only slope or intercept by using the par parameter. You can also provide the rope and bins (number of bins in the histogram) parameters.
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Raw data for the tested group.
#'
#' @examples
#' \donttest{
#' # priors
#' mu_prior <- b_prior(family="normal", pars=c(0, 100))
#' sigma_prior <- b_prior(family="uniform", pars=c(0, 500))
#'
#' # attach priors to relevant parameters
#' priors <- list(c("mu_a", mu_prior),
#'                c("sigma_a", sigma_prior),
#'                c("mu_b", mu_prior),
#'                c("sigma_b", sigma_prior),
#'                c("mu_s", sigma_prior),
#'                c("sigma_s", sigma_prior))
#'
#'
#' # generate data and fit
#' x <- vector()
#' y <- vector()
#' s <- vector()
#' for (i in 1:5) {
#'   x <- c(x, rep(1:10, 2))
#'   y <- c(y, rnorm(20, mean=1:10, sd=2))
#'   s <- c(s, rep(i, 20))
#' }
#'
#' fit1 <- b_linear(x=x, y=y, s=s, priors=priors, chains=1)
#'
#' fit2 <- b_linear(x=x, y=-2*y, s=s, priors=priors, chains=1)
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
#' # extract parameter values from the fit
#' parameters <- get_parameters(fit1)
#'
#' # extract parameter values on the bottom (subject) level from the fit
#' subject_parameters <- get_subject_parameters(fit1)
#'
#' # compare means between two fits
#' compare_means(fit1, fit2=fit2)
#'
#' # compare means between two fits, use a rope interval for intercept and slope
#' compare_means(fit1, fit2=fit2, rope_intercept=0.5, rope_slope=0.2)
#'
#' # visualize difference in means between two fits
#' plot_means_difference(fit1, fit2=fit2)
#'
#' # visualize difference in means between two fits,
#' # use a rope interval for intercept and slope,
#' # set the number of bins in the histogram
#' plot_means_difference(fit1, fit2=fit2, rope_intercept=0.5, rope_slope=0.2, bins=20)
#'
#' # visualize difference in means between two fits, compare only slope
#' plot_means_difference(fit1, fit2=fit2, par="slope")
#'
#' # visualize means of a single fit
#' plot_means(fit1)
#'
#' # visualize means of two fits
#' plot_means(fit1, fit2=fit2)
#'
#' # visualize means of two fits, plot slope only
#' plot_means(fit1, fit2=fit2, par="slope")
#'
#' # draw samples from distributions underlying two fits and compare them,
#' # use a rope interval for intercept and slope
#' compare_distributions(fit1, fit2=fit2, rope_intercept=0.5, rope_slope=0.2)
#'
#' # visualize the distribution underlying a fit
#' plot_distributions(fit1)
#'
#' # visualize distributions underlying two fits
#' plot_distributions(fit1, fit2=fit2)
#'
#' # visualize distributions underlying two fits, plot slope only
#' plot_distributions(fit1, fit2=fit2, par="slope")
#'
#' # visualize difference between distributions underlying two fits
#' plot_distributions_difference(fit1, fit2=fit2)
#'
#' # visualize difference between distributions underlying two fits,
#' # use a rope interval for intercept and slope,
#' # set the number of bins in the histogram
#' plot_distributions_difference(fit1, fit2=fit2, rope_intercept=0.5, rope_slope=0.2, bins=20)
#'
#' # visualize difference between distributions underlying two fits, plot slope only
#' plot_distributions_difference(fit1, fit2=fit2, par="slope")
#' }
#'
linear_class <- setClass(
  "linear_class",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)


#' @title summary
#' @description \code{summary} prints a summary of the Bayesian linear model fit.
#' @param object linear_class object.
#' @exportMethod summary
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="summary", signature(object="linear_class"), definition=function(object) {
  # get means
  alpha <- mean(object@extract$mu_a)
  beta <- mean(object@extract$mu_b)
  sigma <- mean(object@extract$mu_s)

  # HDI
  alpha_hdi <- mcmc_hdi(object@extract$mu_a)
  beta_hdi <- mcmc_hdi(object@extract$mu_b)
  sigma_hdi <- mcmc_hdi(object@extract$mu_s)

  # print
  cat(sprintf("intercept (alpha):\t%.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
              alpha, mcmcse::mcse(object@extract$mu_a)$se, alpha_hdi[1], alpha_hdi[2]))
  cat(sprintf("slope (beta):\t\t%.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
              beta, mcmcse::mcse(object@extract$mu_b)$se, beta_hdi[1], beta_hdi[2]))
  cat(sprintf("sigma:\t\t\t%.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
              sigma, mcmcse::mcse(object@extract$mu_s)$se, sigma_hdi[1], sigma_hdi[2]))
})


#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian linear model fit.
#' @param object linear_class object.
#' @exportMethod show
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="show", signature(object="linear_class"), definition=function(object) {
  # print
  show(object@fit)
})


#' @title plot
#' @description \code{plot} plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subject level (subjects=TRUE) or on the group level (subjects=FALSE).
#' @param x linear_class object.
#' @param y empty dummy variable, ignore this.
#' @param ... subjects - plot fits on a subject level (default = TRUE).
#' @exportMethod plot
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="plot", signature(x="linear_class", y="missing"), definition=function(x, ...) {
  return(plot_fit(object=x, ...))
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subject level (subjects=TRUE) or on the group level (subjects=FALSE).
#' @param object linear_class object.
#' @param ... subjects - plot fits on a subject level (default = TRUE).
#' @rdname linear_class-plot_fit
#' @aliases plot_fit_linear
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="plot_fit", signature(object="linear_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  slope <- intercept <- s <- x <- y <- NULL

  arguments <- list(...)

  # plot on a subject level?
  subjects <- TRUE
  if (!is.null(arguments$subjects)) {
    subjects <- arguments$subjects
  }

  # data
  df_data <- data.frame(x=object@data$x, y=object@data$y, s=object@data$s)

  n <- length(unique(df_data$s))

  x_min <- floor(min(df_data$x))
  y_min <- floor(min(df_data$y))
  x_max <- ceiling(max(df_data$x))
  y_max <- ceiling(max(df_data$y))

  diff_x <- x_max - x_min
  x_min <- x_min - 0.1*diff_x
  x_max <- x_max + 0.1*diff_x
  diff_y <- y_max - y_min
  y_min <- y_min - 0.1*diff_y
  y_max <- y_max + 0.1*diff_y

  # mean per subject
  df_data <- df_data %>% group_by(s, x) %>% summarize(y=mean(y, na.rm=TRUE))

  if (!subjects) {
    m <- min(100, length(object@extract$mu_a))
    # fit
    df_fit <- data.frame(intercept=object@extract$mu_a,
                         slope=object@extract$mu_b)
    df_fit <- sample_n(df_fit, m)

    graph <- ggplot() +
      geom_point(data=df_data, aes(x=x, y=y), color="#3182bd", alpha=0.4, shape=16) +
      geom_abline(data=df_fit, aes(slope=slope, intercept=intercept), color="#3182bd", alpha=0.1, size=1) +
      xlim(x_min, x_max) +
      ylim(y_min, y_max) +
      ylab("response") +
      xlab("question index")
  } else {
    m <- min(20, length(object@extract$mu_a))
    # fits
    df_fit <- NULL
    for (i in 1:n) {
      df <- data.frame(intercept=object@extract$alpha[,i],
                       slope=object@extract$beta[,i],
                       s=i)
      df <- sample_n(df, m)
      df_fit <- rbind(df_fit, df)
    }

    # ncol
    n_col <- ceiling(sqrt(n))

    # density per subject
    graph <- ggplot() +
      geom_point(data=df_data, aes(x=x, y=y), color="#3182bd", shape=16, alpha=0.4) +
      geom_abline(data=df_fit, aes(slope=slope, intercept=intercept), color="#3182bd", alpha=0.2, size=1) +
      facet_wrap(. ~ s, ncol=n_col) +
      ylab("response") +
      xlab("question index")
  }

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object linear_class object.
#' @rdname linear_class-plot_trace
#' @aliases plot_trace_linear
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="plot_trace", signature(object="linear_class"), definition=function(object) {
  rstan::traceplot(object@fit, pars=c("mu_a", "mu_b", "mu_s"), inc_warmup=TRUE)
})


#' @title get_parameters
#' @description \code{get_parameters} returns a dataframe with values of fitted parameters.
#' @param object linear_class object.
#' @rdname linear_class-get_parameters
#' @aliases get_parameters_linear_class
#' @return A data frame with parameter values.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="get_parameters", signature(object="linear_class"), definition=function(object) {
  df <- data.frame(slope=object@extract$mu_a,
                   intercept=object@extract$mu_b,
                   sigma=object@extract$mu_s)

  return(df)
})


#' @title get_subject_parameters
#' @description \code{get_subject_parameters} returns a dataframe with values of fitted parameters for each subject in the hierarchical model.
#' @param object linear_class object.
#' @rdname linear_class-get_subject_parameters
#' @aliases get_subject_parameters_linear_class
#' @return A data frame with parameter values.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="get_subject_parameters", signature(object="linear_class"), definition=function(object) {
  df <- data.frame(slope=numeric(), intercept=numeric(), sigma=numeric(), subject=numeric())

  n <- length(unique(object@data$s))

  for (i in 1:n) {
    df_subject <- data.frame(slope = object@extract$alpha[,i],
                             intercept = object@extract$beta[,i],
                             sigma = object@extract$sigma[,i],
                             subject = i)

    df <- rbind(df, df_subject)
  }

  return(df)
})


#' @title compare_means
#' @description \code{compare_means} prints difference in intercept and slope between two groups.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object, rope_intercept and rope_slope - regions of practical equivalence.
#' @rdname linear_class-compare_means
#' @aliases compare_meanslinear
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="compare_means", signature(object="linear_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_means function are invalid, compare_means(linear_class, fit2=linear_class) is required! You can also provide the rope parameters, e.g. compare_means(linear_class, fit2=linear_class, rope_intercept=numeric, rope_slope=numeric)."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept <- arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope <- arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  # first group data
  intercept1 <- object@extract$mu_a
  slope1 <- object@extract$mu_b

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    intercept2 <- fit2@extract$mu_a
    slope2 <- fit2@extract$mu_b

    cat("---------- Intercept ----------\n")
    intercept <- difference(y1=intercept1, y2=intercept2, rope=rope_intercept)

    cat("\n---------- Slope ----------\n")
    slope <- difference(y1=slope1, y2=slope2, rope=rope_slope)

    cat("\n")
    return(rbind(intercept, slope))
  } else {
    stop(wrong_arguments)
  }
})


#' @title plot_means_difference
#' @description \code{plot_means_difference} plots difference between two groups.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object, par - specific parameter of comparison (slope or intercept), rope_intercept and rope_slope - regions of practical equivalence, bins - number of bins in the histogram.
#' @rdname linear_class-plot_means_difference
#' @aliases plot_means_difference_linear
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="plot_means_difference", signature(object="linear_class"), definition=function(object, ...) {
  # extract additional parameters
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_means_difference function are invalid, plot_means_difference(linear_class, fit2=linear_class) is required! You can optionallly provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_means_difference(linear_class, fit2=linear_class, rope_intercept=numeric, rope_slope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # compare through slope or intercept only
  par <- NULL
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "slope" || par == "intercept")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are slope and intercept! Using the default setting for comparison.", par)
      warning(w)
      par <- NULL
    } else {
      cat(sprintf("\n---------- Using only the %s parameter. ----------\n\n", par))
    }
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept <- arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope <- arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  # first group data
  intercept1 <- object@extract$mu_a
  slope1 <- object@extract$mu_b

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    intercept2 <- fit2@extract$mu_a
    slope2 <- fit2@extract$mu_b

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph_intercept <- plot_difference(y1=intercept1, y2=intercept2, rope=rope_intercept, bins=bins)
    graph_intercept <- graph_intercept +
      ggtitle("Intercept") +
      theme(plot.title=element_text(hjust=0.5))

    graph_slope <- plot_difference(y1=slope1, y2=slope2, rope=rope_slope, bins=bins)
    graph_slope <- graph_slope +
      ggtitle("Slope") +
      theme(plot.title=element_text(hjust=0.5))

    if (is.null(par)) {
      graph <- cowplot::plot_grid(graph_intercept, graph_slope, ncol=2, nrow=1, scale=0.9)
    } else if (par == "slope") {
      graph <- graph_slope
    } else if (par == "intercept") {
      graph <- graph_intercept
    }

    return(graph)
  } else {
    stop(wrong_arguments)
  }
})


#' @title plot_means
#' @description \code{plot_means} plots means or the first and the second group means.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object, par - plot a specific parameter (slope or intercept).
#' @rdname linear_class-plot_means
#' @aliases plot_means_linear
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="plot_means", signature(object="linear_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  intercept <- slope <- NULL

  # extract additional parameters
  arguments <- list(...)

  # compare through slope or intercept only
  par <- NULL
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "slope" || par == "intercept")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are slope and intercept! Using the default setting for comparison.", par)
      warning(w)
      par <- NULL
    } else {
      cat(sprintf("\n---------- Using only the %s parameter. ----------\n\n", par))
    }
  }

  # first group data
  df1 <- data.frame(intercept=object@extract$mu_a, slope=object@extract$mu_b)

  # limits
  x_min_intercept <- min(df1$intercept)
  x_max_intercept <- max(df1$intercept)
  x_min_slope <- min(df1$slope)
  x_max_slope <- max(df1$slope)

  # plot
  graph_intercept <- ggplot() +
    geom_density(data=df1, aes(x=intercept), fill="#3182bd", alpha=0.4, color=NA)
  graph_slope <- ggplot() +
    geom_density(data=df1, aes(x=slope), fill="#3182bd", alpha=0.4, color=NA)

  # second group data
  df2 <- NULL
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }

      df2 <- data.frame(intercept=fit2@extract$mu_a, slope=fit2@extract$mu_b)

      # limits
      x_min_intercept <- min(x_min_intercept, df2$intercept)
      x_max_intercept <- max(x_max_intercept, df2$intercept)
      x_min_slope <- min(x_min_slope, df2$slope)
      x_max_slope <- max(x_max_slope, df2$slope)

      # plot
      graph_intercept <- graph_intercept +
        geom_density(data=df2, aes(x=intercept), fill="#ff4e3f", alpha=0.4, color=NA)
      graph_slope <- graph_slope +
        geom_density(data=df2, aes(x=slope), fill="#ff4e3f", alpha=0.4, color=NA)
    }
  }

  # limits
  diff <- x_max_intercept - x_min_intercept
  x_min_intercept <- x_min_intercept - 0.1*diff
  x_max_intercept <- x_max_intercept + 0.1*diff
  diff <- x_max_slope - x_min_slope
  x_min_slope <- x_min_slope - 0.1*diff
  x_max_slope <- x_max_slope + 0.1*diff

  # plot
  graph_intercept <- graph_intercept +
    xlab("intercept") +
    xlim(x_min_intercept, x_max_intercept)
  graph_slope <- graph_slope +
    xlab("slope") +
    xlim(x_min_slope, x_max_slope)

  if (is.null(par)) {
    graph <- cowplot::plot_grid(graph_intercept, graph_slope, ncol=2, nrow=1, scale=0.9)
  } else if (par == "slope") {
    graph <- graph_slope
  } else if (par == "intercept") {
    graph <- graph_intercept
  }

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object, rope_intercept and rope_slope - regions of practical equivalence.
#' @rdname linear_class-compare_distributions
#' @aliases compare_distributions_linear
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="compare_distributions", signature(object="linear_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(linear_class, fit2=linear_class) is required! You can also provide the rope parameter, e.g. compare_distributions(linear_class, fit2=linear_class, rope_intercept=numeric, rope_slope=numeric)."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept <- arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope <- arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  n <- 100000

  # first group data
  mu_intercept1 <- mean(object@extract$mu_a)
  sigma_intercept1 <- mean(object@extract$sigma_a)
  intercept1 <- stats::rnorm(n, mean=mu_intercept1, sd=sigma_intercept1)

  mu_slope1 <- mean(object@extract$mu_b)
  sigma_slope1 <- mean(object@extract$sigma_b)
  slope1 <- stats::rnorm(n, mean=mu_slope1, sd=sigma_slope1)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    mu_intercept2 <- mean(fit2@extract$mu_a)
    sigma_intercept2 <- mean(fit2@extract$sigma_a)
    intercept2 <- stats::rnorm(n, mean=mu_intercept2, sd=sigma_intercept2)

    mu_slope2 <- mean(fit2@extract$mu_b)
    sigma_slope2 <- mean(fit2@extract$sigma_b)
    slope2 <- stats::rnorm(n, mean=mu_slope2, sd=sigma_slope2)

    cat("---------- Intercept ----------\n")
    intercept <- difference(y1=intercept1, y2=intercept2, rope=rope_intercept)

    cat("\n---------- Slope ----------\n")
    slope <- difference(y1=slope1, y2=slope2, rope=rope_slope)

    cat("\n")
    return(rbind(intercept, slope))
  } else {
    stop(wrong_arguments)
  }
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the fitted distribution, for one or two fits.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object.
#' @rdname linear_class-plot_distributions
#' @aliases plot_distributions_linear
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="plot_distributions", signature(object="linear_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  slope <- intercept <- group <- y <- y_min <- NULL

  # first group mean data
  df_mean <- data.frame(intercept=mean(object@extract$mu_a), slope=mean(object@extract$mu_b), group="1")

  # first group samples
  n <- min(100, length(object@extract$mu_a))
  df <- data.frame(intercept=object@extract$mu_a, slope=object@extract$mu_b, group="1")
  df <- sample_n(df, n)

  # limits
  x_min <- min(object@data$x)
  x_max <- max(object@data$x)
  y_min <- min(object@data$y)
  y_max <- max(object@data$y)

  # second group data
  graph <- ggplot()
  arguments <- list(...)
  if (length(arguments) > 0) {
    if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
      # provided another fit
      if (!is.null(arguments$fit2)) {
        fit2 <- arguments$fit2
      } else {
        fit2 <- arguments[[1]]
      }
      # second group mean data
      df_mean <- rbind(df_mean, data.frame(intercept=mean(fit2@extract$mu_a), slope=mean(fit2@extract$mu_b), group="2"))

      # second group samples
      n <- min(100, length(fit2@extract$mu_a))
      df2 <- data.frame(intercept=fit2@extract$mu_a, slope=fit2@extract$mu_b, group="2")
      df2 <- sample_n(df2, n)
      df <- rbind(df, df2)

      # limits
      x_min <- min(x_min, fit2@data$x)
      x_max <- max(x_max, fit2@data$x)
      y_min <- min(y_min, fit2@data$y)
      y_max <- max(y_max, fit2@data$y)
    }
  }

  diff_x <- x_max - x_min
  x_min <- x_min - 0.1*diff_x
  x_max <- x_max + 0.1*diff_x
  diff_y <- y_max - y_min
  y_min <- y_min - 0.1*diff_y
  y_max <- y_max + 0.1*diff_y

  graph <- ggplot() +
    geom_abline(data=df, aes(slope=slope, intercept=intercept, color=group), alpha=0.1, size=1) +
    geom_abline(data=df_mean, aes(slope=slope, intercept=intercept, color=group), size=1.5) +
    scale_color_manual(values=c("#3182bd", "#ff4e3f")) +
    xlim(x_min, x_max) +
    ylim(y_min, y_max) +
    xlab("") +
    ylab("")

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} visualizes the difference between two groups.
#' @param object linear_class object.
#' @param ... fit2 - a second linear_class object,  par - specific parameter of comparison (slope or intercept), rope_intercept and rope_slope - regions of practical equivalence, bins - number of bins in the histogram.
#' @rdname linear_class-plot_distributions_difference
#' @aliases plot_distributions_difference_linear
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?linear_class
#'
setMethod(f="plot_distributions_difference", signature(object="linear_class"), definition=function(object, ...) {
  # extract additional parameters
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(linear_class, fit2=linear_class) is required! You can also provide the rope and bins (number of bins in the histogram) parameters, e.g. plot_distributions_difference(linear_class, fit2=linear_class, rope_intercept=numeric, rope_slope=numeric, bins=numeric)."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # compare through slope or intercept only
  par <- NULL
  if (!is.null(arguments$par)) {
    par <- arguments$par

    if (!(par == "slope" || par == "intercept")) {
      w <- sprintf("Parameter %s not recognized, parameters used in this model are slope and intercept! Using the default setting for comparison.", par)
      warning(w)
      par <- NULL
    } else {
      cat(sprintf("\n---------- Using only the %s parameter. ----------\n\n", par))
    }
  }

  # prepare rope
  rope_intercept <- NULL
  if (!is.null(arguments$rope_intercept)) {
    rope_intercept <- arguments$rope_intercept
  }
  rope_intercept <- prepare_rope(rope_intercept)

  rope_slope <- NULL
  if (!is.null(arguments$rope_slope)) {
    rope_slope <- arguments$rope_slope
  }
  rope_slope <- prepare_rope(rope_slope)

  n <- 100000

  # first group data
  mu_intercept1 <- mean(object@extract$mu_a)
  sigma_intercept1 <- mean(object@extract$sigma_a)
  intercept1 <- stats::rnorm(n, mean=mu_intercept1, sd=sigma_intercept1)

  mu_slope1 <- mean(object@extract$mu_b)
  sigma_slope1 <- mean(object@extract$sigma_b)
  slope1 <- stats::rnorm(n, mean=mu_slope1, sd=sigma_slope1)

  # second group data
  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "linear_class") {
    # provided another fit
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
    mu_intercept2 <- mean(fit2@extract$mu_a)
    sigma_intercept2 <- mean(fit2@extract$sigma_a)
    intercept2 <- stats::rnorm(n, mean=mu_intercept2, sd=sigma_intercept2)

    mu_slope2 <- mean(fit2@extract$mu_b)
    sigma_slope2 <- mean(fit2@extract$sigma_b)
    slope2 <- stats::rnorm(n, mean=mu_slope2, sd=sigma_slope2)

    # bins in the histogram
    bins <- 30
    if (!is.null(arguments$bins)) {
      bins <- arguments$bins
    }

    # call plot difference from shared plots
    graph_intercept <- plot_difference(y1=intercept1, y2=intercept2, rope=rope_intercept, bins=bins)
    graph_intercept <- graph_intercept +
      ggtitle("Intercept") +
      theme(plot.title=element_text(hjust=0.5))

    graph_slope <- plot_difference(y1=slope1, y2=slope2, rope=rope_slope, bins=bins)
    graph_slope <- graph_slope +
      ggtitle("Slope") +
      theme(plot.title=element_text(hjust=0.5))

    if (is.null(par)) {
      graph <- cowplot::plot_grid(graph_intercept, graph_slope, ncol=2, nrow=1, scale=0.9)
    } else if (par == "slope") {
      graph <- graph_slope
    } else if (par == "intercept") {
      graph <- graph_intercept
    }

    return(graph)
  } else {
    stop(wrong_arguments)
  }
})
