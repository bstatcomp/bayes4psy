#' @title color_class
#' @import ggplot2
#' @description An S4 class for storing results of Bayesian color model.
#'
#' \strong{Functions}
#'
#' summary(`color_class`): prints a summary of the fit.
#'
#' print(`color_class`): prints a more detailed summary of the fit
#'
#' show(`color_class`): prints a more detailed summary of the fit.
#'
#' plot(`color_class`): plots fitted model against the data. Use this function to explore the quality of your fit. You can compare fit with underlying data only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_fit(`color_class`): plots fitted model against the data. Use this function to explore the quality of your fit. You can compare fit with underlying data only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_trace(`color_class`): traceplot for main fitted model parameters.
#'
#' get_parameters(`color_class`): returns a dataframe with values of fitted parameters.
#'
#' compare_means(`color_class`, fit2=`color_class`): prints color difference between two fits. You can also provide the rope parameter or execute the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' compare_means(`color_class`, rgb=`vector`): prints color difference between a fit and a color defined with rgb components. You can also provide the rope parameter or execute the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' compare_means(`color_class`, hsv=`vector`): prints color difference between a fit and a color defined with hsv components. You can also provide the rope parameter or execute the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_means_difference(`color_class`, fit2=`color_class`): a visualization of the difference between two fits You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_means_difference(`color_class`, rgb=`vector`): a visualization of the difference between a fit and a color defined with rgb components. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_means_difference(`color_class`, hsv=`vector`): a visualization of the difference between a fit and a color defined with hsv components. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_means(`color_class`): plots density of means. You can also visualize the density only for chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_means(`color_class`, fit2=`color_class`): plots density for the first and the second group means. You can also visualize the density only for chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_means(`color_class`, rgb=`vector`): plots density for the first and a color defined with rgb components. You can also visualize the density only for chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_means(`color_class`, hsv=`vector`): plots density for the first and a color defined with hsv components. You can also visualize the density only for chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' compare_distributions(`color_class`, fit2=`color_class`): draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group. You can also provide the rope parameter or execute the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' compare_distributions(`color_class`, rgb=`vector`): draws samples from distribution of the first group and compares them against a color defined with rgb components. You can also provide the rope parameter or execute the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' compare_distributions(`color_class`, hsv=`vector`): draws samples from distribution of the first group and compares them against a color defined with hsv components. You can also provide the rope parameter or execute the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_distributions(`color_class`): a visualization of the fitted distribution. You can visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_distributions(`color_class`, fit2=`color_class`): a visualization of two fitted distributions. You can visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_distributions(`color_class`, rgb=`vector`): a visualization of the fitted distribution and a color defined with rgb components. You can visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_distributions(`color_class`, hsv=`vector`): a visualization of the fitted distribution and a color defined with hsv components. You can visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_distributions_difference(`color_class`, fit2=`color_class`): a visualization of the difference between the distribution of the first fit and the second fit. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_distributions_difference(`color_class`, rgb=`vector`): a visualization of the difference between the distribution of the first fit and a color defined with rgb components. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_distributions_difference(`color_class`, hsv=`vector`): a visualization of the difference between the distribution of the first fit and a color defined with hsv components. You can also provide the rope and bins (number of bins in the histogram) parameters or visualize the comparison only through chosen color components (r, g, b, h, s, v) by using the pars parameter.
#'
#' plot_hsv(`color_class`): plots fitted model against the data. Use this function to explore the quality of your fit thorough a circular visualization of hsv color components.
#'
#' plot_fit_hsv(`color_class`): plots fitted model against the data. Use this function to explore the quality of your fit thorough a circular visualization of hsv color components.
#'
#' plot_means_hsv(`color_class`): a visualization of the difference between means of two fits through a circular visualization of hsv color components. You can also compare fit means with colors defined through rgb or hsv components (as points or as lines on the visualization).
#'
#' plot_distributions_hsv(`color_class`): a visualization of distributions of one or two fits thorough a circular visualization of hsv color components. You can also compare fit means with colors defined through rgb or hsv components (as points or as lines on the visualization).
#'
#' @slot extract Extract from Stan fit.
#' @slot fit Stan fit.
#' @slot data Data on which the fit is based.
#'
#' @examples
#' \donttest{
#' # priors for rgb
#' mu_prior <- b_prior(family="uniform", pars=c(0, 255))
#' sigma_prior <- b_prior(family="uniform", pars=c(0, 100))
#'
#' # attach priors to relevant parameters
#' priors_rgb <- list(c("mu_r", mu_prior),
#'                    c("sigma_r", sigma_prior),
#'                    c("mu_g", mu_prior),
#'                    c("sigma_g", sigma_prior),
#'                    c("mu_b", mu_prior),
#'                    c("sigma_b", sigma_prior))
#'
#'
#' # generate data (rgb) and fit
#' r <- as.integer(rnorm(100, mean=250, sd=20))
#' r[r > 255] <- 255
#' r[r < 0] <- 0
#'
#' g <- as.integer(rnorm(100, mean=20, sd=20))
#' g[g > 255] <- 255
#' g[g < 0] <- 0
#'
#' b <- as.integer(rnorm(100, mean=40, sd=20))
#' b[b > 255] <- 255
#' b[b < 0] <- 0
#'
#' colors <- data.frame(r=r, g=g, b=b)
#'
#' fit1 <- b_color(colors=colors, priors=priors_rgb, chains=1)
#'
#'
#' # priors for hsv
#' h_prior <- b_prior(family="uniform", pars=c(0, 2*pi))
#' sv_prior <- b_prior(family="uniform", pars=c(0, 1))
#' kappa_prior <- b_prior(family="uniform", pars=c(0, 500))
#' sigma_prior <- b_prior(family="uniform", pars=c(0, 1))
#'
#' # attach priors to relevant parameters
#' priors_hsv <- list(c("mu_h", h_prior),
#'                    c("kappa_h", kappa_prior),
#'                    c("mu_s", sv_prior),
#'                    c("sigma_s", sigma_prior),
#'                    c("mu_v", sv_prior),
#'                    c("sigma_v", sigma_prior))
#'
#' # generate data (hsv) and fit
#' h <- rnorm(100, mean=2*pi/3, sd=0.5)
#' h[h > 2*pi] <- 2*pi
#' h[h < 0] <- 0
#'
#' s <- rnorm(100, mean=0.9, sd=0.2)
#' s[s > 1] <- 1
#' s[s < 0] <- 0
#'
#' v <- rnorm(100, mean=0.9, sd=0.2)
#' v[v > 1] <- 1
#' v[v < 0] <- 0
#'
#' colors <- data.frame(h=h, s=s, v=v)
#'
#' fit2 <- b_color(colors=colors, hsv=TRUE, priors=priors_hsv, chains=1)
#'
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
#' # specify only a subset of parameters for visualization
#' plot(fit1, pars=c("h", "s", "v"))
#' plot_fit(fit1, pars=c("h", "s", "v"))
#'
#' # traceplot of the fitted parameters
#' plot_trace(fit1)
#'
#' # extract parameter values from the fit
#' parameters <- get_parameters(fit1)
#'
#' # compare means between two fits
#' compare_means(fit1, fit2=fit2)
#'
#' # compare means between two fits,
#' # specify only a subset of parameters for comparison
#' compare_means(fit1, fit2=fit2, pars=c("h", "s", "v"))
#'
#' # compare means of a fit with an rgb defined color
#' compare_means(fit1, rgb=c(255, 0, 0))
#'
#' # compare means of a fit with an hsv defined color
#' compare_means(fit1, hsv=c(pi/2, 1, 1))
#'
#' # visualize difference in means between two fits
#' plot_means_difference(fit1, fit2=fit2)
#'
#' # visualize difference in means between two fits,
#' # specify only a subset of parameters for comparison, use a rope interval
#' plot_means_difference(fit1, fit2=fit2, pars=c("r", "g", "b"), rope=10)
#'
#' # visualize difference in means between a fit and an rgb defined color
#' plot_means_difference(fit1, rgb=c(255, 0, 0))
#'
#' # visualize difference in means between a fit and an hsv defined color
#' plot_means_difference(fit1, hsv=c(pi/2, 1, 1))
#'
#' # visualize means of a single fit
#' plot_means(fit1)
#'
#' # visualize means of two fits
#' plot_means(fit1, fit2=fit2)
#'
#' # visualize means of two fits,
#' # specify only a subset of parameters for visualization
#' plot_means(fit1, fit2=fit2, pars=c("h", "s", "v"))
#'
#' # visualize means of a single fit and an rgb defined color
#' plot_means(fit1, rgb=c(255, 0, 0))
#'
#' # visualize means of a single fit and an an hsv defined color
#' plot_means(fit1, hsv=c(pi/2, 1, 1))
#'
#' # draw samples from distributions underlying two fits and compare them
#' compare_distributions(fit1, fit2=fit2)
#'
#' # draw samples from distributions underlying two fits and compare them,
#' # specify only a subset of parameters for comparison, use a rope interval
#' compare_distributions(fit1, fit2=fit2, pars=c("r", "g", "b"), rope=10)
#'
#' # draw samples from a distribution underlying the fits,
#' # compare them with an rgb defined color
#' compare_distributions(fit1, rgb=c(255, 0, 0))
#'
#' # draw samples from a distribution underlying the fits,
#' # compare them with an hsv defined color
#' compare_distributions(fit1, hsv=c(pi/2, 1, 1))
#'
#' # visualize the distribution underlying a fit
#' plot_distributions(fit1)
#'
#' # visualize distributions underlying two fits
#' plot_distributions(fit1, fit2=fit2)
#'
#' # visualize distributions underlying two fits,
#' # specify only a subset of parameters for visualization
#' plot_distributions(fit1, fit2=fit2, pars=c("h", "s", "v"))
#'
#' # visualize the distribution underlying a fit, and an rgb defined color
#' plot_distributions(fit1, rgb=c(255, 0, 0))
#'
#' # visualize the distribution underlying a fit, and an hsv defined color
#' plot_distributions(fit1, hsv=c(pi/2, 1, 1))
#'
#' # visualize difference between distributions underlying two fits
#' plot_distributions_difference(fit1, fit2=fit2)
#'
#' # visualize difference between distributions underlying two fits
#' # specify only a subset of parameters for comparison, use a rope interval
#' plot_distributions_difference(fit1, fit2=fit2, pars=c("r", "g", "b"), rope=10)
#'
#' # visualize difference between the distributions underlyin a fit,
#' # and an rgb defined color
#' plot_distributions_difference(fit1, rgb=c(255, 0, 0))
#'
#' # visualize difference between the distributions underlyin a fit,
#' # and an hsv defined color
#' plot_distributions_difference(fit1, hsv=c(pi/2, 1, 1))
#'
#' # plot the fitted distribution for hue against the hue data
#' plot_hsv(fit1)
#'
#' # plot the fitted distribution for hue against the hue data
#' plot_fit_hsv(fit1)
#'
#' # visualize hue means of a single fit
#' plot_means_hsv(fit1)
#'
#' # visualize hue means of two fits
#' plot_means_hsv(fit1, fit2=fit2)
#'
#' # visualize hue means of two fits, add annotation points and lines,
#' # hsv parameter determines whether annotations are defined in hsv or rgb
#' lines <- list()
#' lines[[1]] <- c(2*pi, 1, 1)
#' lines[[2]] <- c(pi/2, 0.5, 0.5)
#'
#' points <- list()
#' points[[1]] <- c(pi, 1, 1)
#'
#' plot_means_hsv(fit1, fit2=fit2, points=points, lines=lines, hsv=TRUE)
#'
#' # visualize the hue distribution underlying a fit
#' plot_distributions_hsv(fit1)
#'
#' # visualize hue distributions underlying two fits
#' plot_distributions_hsv(fit1, fit2=fit2)
#'
#' # visualize hue distributions of two fits, add annotation points and lines,
#' # hsv parameter determines whether annotations are defined in hsv or rgb
#' lines <- list()
#' lines[[1]] <- c(2*pi, 1, 1)
#' lines[[2]] <- c(pi/2, 0.5, 0.5)
#'
#' points <- list()
#' points[[1]] <- c(pi, 1, 1)
#'
#' plot_distributions_hsv(fit1, fit2=fit2, points=points, lines=lines, hsv=TRUE)
#' }
#'
color_class <- setClass(
  "color_class",
  slots = c(
    extract = "list",
    fit = "stanfit",
    data = "list"
  ),
  contains = "b_results"
)


#' @title summary
#' @description \code{summary} prints summary of the Bayesian color fit.
#' @param object color_class object.
#' @exportMethod summary
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="summary", signature(object="color_class"), definition=function(object) {
  # get means
  mu_r <- mean(object@extract$mu_r)
  sigma_r <- mean(object@extract$sigma_r)
  mu_g <- mean(object@extract$mu_g)
  sigma_g <- mean(object@extract$sigma_g)
  mu_b <- mean(object@extract$mu_b)
  sigma_b <- mean(object@extract$sigma_b)

  mu_h <- mean(preprocess_circular(object@extract$mu_h))
  kappa_h <- mean(object@extract$kappa_h)
  mu_s <- mean(object@extract$mu_s)
  sigma_s <- mean(object@extract$sigma_s)
  mu_v <- mean(object@extract$mu_v)
  sigma_v <- mean(object@extract$sigma_v)

  # HDI
  mu_r_hdi <- mcmc_hdi(object@extract$mu_r)
  sigma_r_hdi <- mcmc_hdi(object@extract$sigma_r)
  mu_g_hdi <- mcmc_hdi(object@extract$mu_g)
  sigma_g_hdi <- mcmc_hdi(object@extract$sigma_g)
  mu_b_hdi <- mcmc_hdi(object@extract$mu_b)
  sigma_b_hdi <- mcmc_hdi(object@extract$sigma_b)

  mu_h_hdi <- mcmc_hdi(preprocess_circular(object@extract$mu_h))
  kappa_h_hdi <- mcmc_hdi(object@extract$kappa_h)
  mu_s_hdi <- mcmc_hdi(object@extract$mu_s)
  sigma_s_hdi <- mcmc_hdi(object@extract$sigma_s)
  mu_v_hdi <- mcmc_hdi(object@extract$mu_v)
  sigma_v_hdi <- mcmc_hdi(object@extract$sigma_v)

  # print)
  cat(sprintf("mu_r:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              mu_r, mcmcse::mcse(object@extract$mu_r)$se, mu_r_hdi[1], mu_r_hdi[2]))
  cat(sprintf("sigma_r:\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              sigma_r, mcmcse::mcse(object@extract$sigma_r)$se, sigma_r_hdi[1], sigma_r_hdi[2]))
  cat(sprintf("mu_g:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              mu_g, mcmcse::mcse(object@extract$mu_g)$se, mu_g_hdi[1], mu_g_hdi[2]))
  cat(sprintf("sigma_g:\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              sigma_g, mcmcse::mcse(object@extract$sigma_g)$se, sigma_g_hdi[1], sigma_g_hdi[2]))
  cat(sprintf("mu_b:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              mu_b, mcmcse::mcse(object@extract$mu_b)$se, mu_b_hdi[1], mu_b_hdi[2]))
  cat(sprintf("sigma_b:\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              sigma_b, mcmcse::mcse(object@extract$sigma_b)$se, sigma_b_hdi[1], sigma_b_hdi[2]))
  cat(sprintf("mu_h:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              mu_h, mcmcse::mcse(preprocess_circular(object@extract$mu_h))$se, mu_h_hdi[1], mu_h_hdi[2]))
  cat(sprintf("kappa_h:\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              kappa_h, mcmcse::mcse(object@extract$kappa_h)$se, kappa_h_hdi[1], kappa_h_hdi[2]))
  cat(sprintf("mu_s:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              mu_s, mcmcse::mcse(object@extract$mu_s)$se, mu_s_hdi[1], mu_s_hdi[2]))
  cat(sprintf("sigma_s:\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              sigma_s, mcmcse::mcse(object@extract$sigma_s)$se, sigma_s_hdi[1], sigma_s_hdi[2]))
  cat(sprintf("mu_v:\t\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              mu_v, mcmcse::mcse(object@extract$mu_v)$se, mu_v_hdi[1], mu_v_hdi[2]))
  cat(sprintf("sigma_v:\t%.2f +/- %.5f\t95%% HDI: [%.2f, %.2f]\n",
              sigma_v, mcmcse::mcse(object@extract$sigma_v)$se, sigma_v_hdi[1], sigma_v_hdi[2]))
})


#' @title show
#' @description \code{show} prints a more detailed summary of the Bayesian color fit.
#' @param object color_class object.
#' @exportMethod show
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="show", signature(object="color_class"), definition=function(object) {
  # print
  show(object@fit)
})


#' @title plot
#' @description \code{plot} plots fitted model against the data. Use this function to explore the quality of your fit. You can compare fit with underlying data only through chosen color components (r, g, b, h, s, v).
#' @param x color_class object.
#' @param y empty dummy variable, ignore this.
#' @param ... pars - components of comparison, a subset of (r, g, b, h, s, v).
#' @exportMethod plot
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot", signature(x="color_class", y="missing"), definition=function(x, ...) {
  return(plot_fit(object=x, ...))
})


#' @title plot_fit
#' @description \code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit. You can compare fit with underlying data only through chosen color components (r, g, b, h, s, v).
#' @param object color_class object.
#' @param ... pars - components of comparison, a subset of (r, g, b, h, s, v).
#' @rdname color_class-plot_fit
#' @aliases plot_fit_color
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_fit", signature(object="color_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- x <- y <- NULL

  # get arguments
  arguments <- list(...)

  # compare through all components or through a subset
  pars <- c("r", "g", "b", "h", "s", "v")
  if (length(arguments) != 0 && !is.null(arguments$pars)) {
    pars <- arguments$pars
  }

  # calculate number of columns and rows
  n_graph <- length(pars)
  nrow <- 1
  ncol <- 1
  if (n_graph > 1) {
    nrow <- ceiling(n_graph / 3)
    ncol <- 3
    if (n_graph == 2 || n_graph == 4) {
      ncol <- 2
    }
  }

  n <- 1000

  # plot
  graphs <- list()
  i <- 1
  for (p in pars) {
    if (p == "r") {
      # first group data
      r_data <- data.frame(x=object@data$r)
      r_mu <- mean(object@extract$mu_r)
      r_sigma <- mean(object@extract$sigma_r)

      # plot
      df_x <- data.frame(value=c(0, 255))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        geom_density(data=r_data, aes(x=x), fill="#808080", alpha=0.5, color=NA) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=r_mu, sd=r_sigma), geom="line", size=1, color="#000000") +
        xlab("value") +
        ggtitle("r") +
        theme(plot.title=element_text(hjust=0.5))

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "g") {
      # first group data
      g_data <- data.frame(x=object@data$g)
      g_mu <- mean(object@extract$mu_g)
      g_sigma <- mean(object@extract$sigma_g)

      # plot
      df_x <- data.frame(value=c(0, 255))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        geom_density(data=g_data, aes(x=x), fill="#808080", alpha=0.5, color=NA) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=g_mu, sd=g_sigma), geom="line", size=1, color="#000000") +
        xlab("value") +
        ggtitle("g") +
        theme(plot.title=element_text(hjust=0.5))

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "b") {
      # first group data
      b_data <- data.frame(x=object@data$b)
      b_mu <- mean(object@extract$mu_b)
      b_sigma <- mean(object@extract$sigma_b)

      # plot
      df_x <- data.frame(value=c(0, 255))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        geom_density(data=b_data, aes(x=x), fill="#808080", alpha=0.5, color=NA) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=b_mu, sd=b_sigma), geom="line", size=1, color="#000000") +
        xlab("value") +
        ggtitle("b") +
        theme(plot.title=element_text(hjust=0.5))

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "h") {
      # first group data
      h_mu <- mean(preprocess_circular(object@extract$mu_h))
      h_data <- data.frame(x=preprocess_circular(object@data$h))
      h_kappa <- mean(object@extract$kappa_h)

      # plot on -pi..pi or 0..2pi
      # plot on -pi..pi or 0..2pi
      if (h_mu < pi/2 & h_mu > -pi/2) {
        x_min <- -pi
        x_max <- pi
      } else if (h_mu < -pi/2) {
        x_min <- -2*pi
        x_max <- 0
      } else {
        x_min <- 0
        x_max <- 2*pi
      }

      # suppress circular warnings
      h_mu <- suppressWarnings(circular::as.circular(h_mu))
      h_kappa <- suppressWarnings(circular::as.circular(h_kappa))

      # get data points
      step <- 2*pi/n
      h_x <- suppressWarnings(circular::as.circular(seq(x_min, x_max, step)))
      h_y <- circular::dvonmises(h_x, h_mu, h_kappa)
      df <- data.frame(x=h_x, y=h_y)

      # plot
      graph <- ggplot() +
        geom_density(data=h_data, aes(x=x), fill="#808080", alpha=0.5, color=NA) +
        geom_line(data=df, aes(x=x, y=y), color="#000000", size=1) +
        xlab("value") +
        ggtitle("h") +
        theme(plot.title=element_text(hjust=0.5))

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "s") {
      # first group data
      s_data <- data.frame(x=object@data$s)
      s_mu <- mean(object@extract$mu_s)
      s_sigma <- mean(object@extract$sigma_s)

      # plot
      df_x <- data.frame(value=c(0, 1))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        geom_density(data=s_data, aes(x=x), fill="#808080", alpha=0.5, color=NA) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=s_mu, sd=s_sigma), geom="line", size=1, color="#000000") +
        xlab("value") +
        ggtitle("s") +
        theme(plot.title=element_text(hjust=0.5))

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "v") {
      # first group data
      v_data <- data.frame(x=object@data$v)
      b_mu <- mean(object@extract$mu_v)
      b_sigma <- mean(object@extract$sigma_v)

      # plot
      df_x <- data.frame(value=c(0, 1))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        geom_density(data=v_data, aes(x=x), fill="#808080", alpha=0.5, color=NA) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=b_mu, sd=b_sigma), geom="line", size=1, color="#000000") +
        xlab("value") +
        ggtitle("v") +
        theme(plot.title=element_text(hjust=0.5))

      graphs[[i]] <- graph
      i <- i + 1
    }
  }

  if (n_graph > 1) {
    graph <- cowplot::plot_grid(plotlist=graphs, ncol=ncol, nrow=nrow, scale=0.9)
  }

  return(graph)
})


#' @title plot_trace
#' @description \code{plot_trace} traceplot for main fitted model parameters.
#' @param object color_class object.
#' @rdname color_class-plot_trace
#' @aliases plot_trace_color
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_trace", signature(object="color_class"), definition=function(object) {
  rstan::traceplot(object@fit, pars=c("mu_r", "mu_g", "mu_b", "mu_h", "mu_s", "mu_v"), inc_warmup=TRUE)
})


#' @title get_parameters
#' @description \code{get_parameters} returns a dataframe with values of fitted parameters.
#' @param object color_class object.
#' @rdname color_class-get_parameters
#' @aliases get_parameters_color_class
#' @return A data frame with parameter values.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="get_parameters", signature(object="color_class"), definition=function(object) {
  df <- data.frame(r=object@extract$mu_r,
                   g=object@extract$mu_g,
                   b=object@extract$mu_b,
                   h=object@extract$mu_h,
                   s=object@extract$mu_s,
                   v=object@extract$mu_v)

  return(df)
})


#' @title compare_means
#' @description \code{compare_means} prints difference in colors between two fits or a fit and a color.
#' @param object color_class object.
#' @param ... fit2 - a second color_class object, rgb - color defined through rgb, hsv - color defined through rgb, rope - region of practical equivalence, pars - components of comparison, a subset of (r, g, b, h, s, v).
#' @rdname color_class-compare_means
#' @aliases compare_means_color
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="compare_means", signature(object="color_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_means function are invalid, compare_means(color_class, fit2=color_class), compare(color_class, rgb=vector) or compare_means(color_class, hsv=vector) is required! You can optionallly provide the rope parameter, e.g. compare_means(color_class, fit2=color_class, rope=numeric). You can also execute the comparison through a subset of color components, e.g. compare_means(color_class, fit2=color_class, pars=c(\"h\", \"s\", \"v\"))."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # comparing with another fit, rgb or hsv
  fit2 <- NULL
  rgb <- NULL
  hsv <- NULL

  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "color_class") {
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
  } else if (!is.null(arguments$rgb)) {
    rgb <- arguments$rgb
    hsv <- grDevices::rgb2hsv(rgb)
  } else if (!is.null(arguments$hsv)) {
    hsv <- arguments$hsv
    rgb <- hsv2rgb(hsv[1], hsv[2], hsv[3])
  }

  # are all null?
  if (is.null(fit2) && is.null(rgb) && is.null(hsv)) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # compare through all components or through a subset
  pars <- c("r", "g", "b", "h", "s", "v")
  if (!is.null(arguments$pars)) {
    pars <- arguments$pars
  }

  result <- NULL
  for (p in pars) {
    if (p == "r") {
      y1 <- object@extract$mu_r

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_r
      } else {
        y2 <- rgb[1]
      }

      cat("\n------------- R component -------------\n")
      r <- difference(y1=y1, y2=y2, rope=rope)
      result <- rbind(result, r)
    } else if (p == "g") {
      y1 <- object@extract$mu_g

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_g
      } else {
        y2 <- rgb[2]
      }

      cat("\n------------- G component -------------\n")
      g <- difference(y1=y1, y2=y2, rope=rope)
      result <- rbind(result, g)
    } else if (p == "b") {
      y1 <- object@extract$mu_b

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_b
      } else {
        y2 <- rgb[3]
      }

      cat("\n------------- B component -------------\n")
      b <- difference(y1=y1, y2=y2, rope=rope)
      result <- rbind(result, b)
    } else if (p == "h") {
      y1 <- object@extract$mu_h

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_h
      } else {
        y2 <- hsv[1]
      }

      cat("\n------------- H component -------------\n")
      h <- circular_difference(y1=y1, y2=y2, rope=rope)
      result <- rbind(result, h)
    } else if (p == "s") {
      y1 <- object@extract$mu_s

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_s
      } else {
        y2 <- hsv[2]
      }

      cat("\n------------- S component -------------\n")
      s <- difference(y1=y1, y2=y2, rope=rope)
      result <- rbind(result, s)
    } else if (p == "v") {
      y1 <- object@extract$mu_v

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_v
      } else {
        y2 <- hsv[3]
      }

      cat("\n------------- V component -------------\n")
      v <- difference(y1=y1, y2=y2, rope=rope)
      result <- rbind(result, v)
    }
  }

  cat("\n")
  return(result)
})


#' @title plot_means_difference
#' @description \code{plot_means_difference} a visualization of the difference between two fits
#' @param object color_class object.
#' @param ... fit2 - a second color_class object, rgb - color defined through rgb, hsv - color defined through rgb, rope - region of practical equivalence, bins - number of bins in the histogram, pars - components of comparison, a subset of (r, g, b, h, s, v).
#' @rdname color_class-plot_means_difference
#' @aliases plot_means_difference_color
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_means_difference", signature(object="color_class"), definition=function(object, ...) {
  # get arguments
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_means_difference function are invalid, plot_means_difference(color_class, fit2=color_class), plot_means_difference(color_class, rgb=vector) or plot_means_difference(color_class, hsv=vector) is required! You can optionallly provide the rope parameter, e.g. plot_means_difference(color_class, fit2=color_class, rope=numeric) or the bins parameter plot_means_difference(color_class, fit2=color_class, bins=numeric). You can also execute the comparison through a subset of color components, e.g. plot_means_difference(color_class, fit2=color_class, pars=c(\"h\", \"s\", \"v\"))."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # comparing with another fit, rgb or hsv
  fit2 <- NULL
  rgb <- NULL
  hsv <- NULL

  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "color_class") {
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
  } else if (!is.null(arguments$rgb)) {
    rgb <- arguments$rgb
    hsv <- grDevices::rgb2hsv(rgb)
  } else if (!is.null(arguments$hsv)) {
    hsv <- arguments$hsv
    rgb <- hsv2rgb(hsv[1], hsv[2], hsv[3])
  }

  # are all null?
  if (is.null(fit2) && is.null(rgb) && is.null(hsv)) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # bins in the histogram
  bins <- 30
  if (!is.null(arguments$bins)) {
    bins <- arguments$bins
  }

  # compare through all components or through a subset
  pars <- c("r", "g", "b", "h", "s", "v")
  if (!is.null(arguments$pars)) {
    pars <- arguments$pars
  }

  # calculate number of columns and rows
  n_graph <- length(pars)
  nrow <- 1
  ncol <- 1
  if (n_graph > 1) {
    nrow <- ceiling(n_graph / 3)
    ncol <- 3
    if (n_graph == 2 || n_graph == 4) {
      ncol <- 2
    }
  }

  # plot
  graphs <- list()
  i <- 1
  for (p in pars) {
    if (p == "r") {
      y1 <- object@extract$mu_r

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_r
      } else {
        y2 <- rgb[1]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow)
      graph <- graph + ggtitle("r") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "g") {
      y1 <- object@extract$mu_g

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_g
      } else {
        y2 <- rgb[2]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow)
      graph <- graph + ggtitle("g") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "b") {
      y1 <- object@extract$mu_b

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_b
      } else {
        y2 <- rgb[3]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow)
      graph <- graph + ggtitle("b") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "h") {
      y1 <- object@extract$mu_h

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_h
      } else {
        y2 <- hsv[1]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, circular=TRUE, nrow=nrow)
      graph <- graph + ggtitle("h") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "s") {
      y1 <- object@extract$mu_s

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_s
      } else {
        y2 <- hsv[2]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow)
      graph <- graph + ggtitle("s") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "v") {
      y1 <- object@extract$mu_v

      if (!is.null(fit2)) {
        y2 <- fit2@extract$mu_v
      } else {
        y2 <- hsv[3]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow)
      graph <- graph + ggtitle("v") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    }
  }

  if (n_graph > 1) {
    graph <- cowplot::plot_grid(plotlist=graphs, ncol=ncol, nrow=nrow, scale=0.9)
  }

  return(graph)
})


#' @title plot_means
#' @description \code{plot_means} plots density of means, the first and the second group means or a constant values in case second group is defined as rgb or hsv color.
#' @param object color_class object.
#' @param ... fit2 - a second color_class object, rgb - color defined through rgb, hsv - color defined through rgb, pars - components of comparison, a subset of (r, g, b, h, s, v).
#' @rdname color_class-plot_means
#' @aliases plot_means_color
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_means", signature(object="color_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- NULL

  # get arguments
  arguments <- NULL
  if (length(list(...))) {
    arguments <- list(...)
  }

  # comparing with another fit, rgb or hsv
  fit2 <- NULL
  rgb <- NULL
  hsv <- NULL

  if (!is.null(arguments) && (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "color_class")) {
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
  } else if (!is.null(arguments$rgb)) {
    rgb <- arguments$rgb
    hsv <- grDevices::rgb2hsv(rgb)
  } else if (!is.null(arguments$hsv)) {
    hsv <- arguments$hsv
    rgb <- hsv2rgb(hsv[1], hsv[2], hsv[3])
  }

  # plot all components or a subset
  pars <- c("r", "g", "b", "h", "s", "v")
  if (!is.null(arguments$pars)) {
    pars <- arguments$pars
  }

  # calculate number of columns and rows
  n_graph <- length(pars)
  nrow <- 1
  ncol <- 1
  if (n_graph > 1) {
    nrow <- ceiling(n_graph / 3)
    ncol <- 3
    if (n_graph == 2 || n_graph == 4) {
      ncol <- 2
    }
  }

  # plot
  graphs <- list()
  i <- 1
  for (p in pars) {
    if (p == "r") {
      # first group data
      r_mu1 <- object@extract$mu_r
      r_df1 <- data.frame(value=r_mu1)

      # plot
      graph <- ggplot() +
        geom_density(data=r_df1, aes(x=value), fill="#808080", alpha=0.5, color=NA) +
        xlab("value") +
        ggtitle("r") +
        theme(plot.title=element_text(hjust=0.5)) +
        xlim(0, 255)

      if (!is.null(fit2)) {
        # second group data
        r_mu2 <- fit2@extract$mu_r
        r_df2 <- data.frame(value=r_mu2)

        graph <- graph +
          stat_density(data=r_df2, aes(x=value), geom="line", color="#000000", size=1)
      } else if (!is.null(rgb)) {
        # predefined color
        r_x2 <- rgb[1]
        r_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 255 * 0.1
        hjust = "center"
        if (r_x2 < hjust_range || r_x2 > (255 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=r_x2, xend=r_x2, y=0, yend=r_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%d", r_x2), x=r_x2, y=r_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "g") {
      # first group data
      g_mu1 <- object@extract$mu_g
      g_df1 <- data.frame(value=g_mu1)

      # plot
      graph <- ggplot() +
        geom_density(data=g_df1, aes(x=value), fill="#808080", alpha=0.5, color=NA) +
        xlab("value") +
        ggtitle("g") +
        theme(plot.title=element_text(hjust=0.5)) +
        xlim(0, 255)

      if (!is.null(fit2)) {
        # second group data
        g_mu2 <- fit2@extract$mu_g
        g_df2 <- data.frame(value=g_mu2)

        graph <- graph +
          stat_density(data=g_df2, aes(x=value), geom="line", color="#000000", size=1)
      } else if (!is.null(rgb)) {
        # predefined color
        g_x2 <- rgb[2]
        g_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 255 * 0.1
        hjust = "center"
        if (g_x2 < hjust_range || g_x2 > (255 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=g_x2, xend=g_x2, y=0, yend=g_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%d", g_x2), x=g_x2, y=g_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "b") {
      # first group data
      b_mu1 <- object@extract$mu_b
      b_df1 <- data.frame(value=b_mu1)

      # plot
      graph <- ggplot() +
        geom_density(data=b_df1, aes(x=value), fill="#808080", alpha=0.5, color=NA) +
        xlab("value") +
        ggtitle("b") +
        theme(plot.title=element_text(hjust=0.5)) +
        xlim(0, 255)

      if (!is.null(fit2)) {
        # second group data
        b_mu2 <- fit2@extract$mu_b
        b_df2 <- data.frame(value=b_mu2)

        graph <- graph +
          stat_density(data=b_df2, aes(x=value), geom="line", color="#000000", size=1)
      } else if (!is.null(rgb)) {
        # predefined color
        b_x2 <- rgb[3]
        b_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 255 * 0.1
        hjust = "center"
        if (b_x2 < hjust_range || b_x2 > (255 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=b_x2, xend=b_x2, y=0, yend=b_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%d", b_x2), x=b_x2, y=b_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "h") {
      # first group data
      h_mu1 <- preprocess_circular(object@extract$mu_h)
      h_df1 <- data.frame(value=h_mu1)

      # plot on -pi..pi, -2pi..0 or 0..2pi
      suppressWarnings(mean_h <- mean(circular::as.circular(h_mu1)))
      if (mean_h < pi/2 & mean_h > -pi/2) {
        x_min <- -pi
        x_max <- pi
      } else if (mean_h < -pi/2) {
        x_min <- -2*pi
        x_max <- 0
      } else {
        x_min <- 0
        x_max <- 2*pi
      }

      # plot
      graph <- ggplot() +
        geom_density(data=h_df1, aes(x=value), fill="#808080", alpha=0.5, color=NA) +
        xlab("value") +
        ggtitle("h") +
        theme(plot.title=element_text(hjust=0.5)) +
        xlim(x_min, x_max)

      if (!is.null(fit2)) {
        # second group data
        h_mu2 <- preprocess_circular(fit2@extract$mu_h, base=h_mu1)
        h_df2 <- data.frame(value=h_mu2)

        graph <- graph +
          stat_density(data=h_df2, aes(x=value), geom="line", color="#000000", size=1)
      } else if (!is.null(hsv)) {
        # predefined color
        h_x2 <- hsv[1]
        h_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 2*pi * 0.1
        hjust = "center"
        if (h_x2 < (x_min + hjust_range) || h_x2 > (x_max - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=h_x2, xend=h_x2, y=0, yend=h_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%.2f", h_x2), x=h_x2, y=h_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "s") {
      # first group data
      s_mu1 <- object@extract$mu_s
      s_df1 <- data.frame(value=s_mu1)

      # plot
      graph <- ggplot() +
        geom_density(data=s_df1, aes(x=value), fill="#808080", alpha=0.5, color=NA) +
        xlab("value") +
        ggtitle("s") +
        theme(plot.title=element_text(hjust=0.5)) +
        xlim(0, 1)

      if (!is.null(fit2)) {
        # second group data
        s_mu2 <- fit2@extract$mu_s
        s_df2 <- data.frame(value=s_mu2)

        graph <- graph +
          stat_density(data=s_df2, aes(x=value), geom="line", color="#000000", size=1)
      } else if (!is.null(hsv)) {
        # predefined color
        s_x2 <- hsv[2]
        s_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 1 * 0.1
        hjust = "center"
        if (s_x2 < hjust_range || s_x2 > (1 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=s_x2, xend=s_x2, y=0, yend=s_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%.2f", s_x2), x=s_x2, y=s_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "v") {
      # first group data
      v_mu1 <- object@extract$mu_v
      v_df1 <- data.frame(value=v_mu1)

      # plot
      graph <- ggplot() +
        geom_density(data=v_df1, aes(x=value), fill="#808080", alpha=0.5, color=NA) +
        xlab("value") +
        ggtitle("v") +
        theme(plot.title=element_text(hjust=0.5)) +
        xlim(0, 1)

      if (!is.null(fit2)) {
        # second group data
        v_mu2 <- fit2@extract$mu_v
        v_df2 <- data.frame(value=v_mu2)

        graph <- graph +
          stat_density(data=v_df2, aes(x=value), geom="line", color="#000000", size=1)
      } else if (!is.null(hsv)) {
        # predefined color
        v_x2 <- hsv[3]
        v_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 1 * 0.1
        hjust = "center"
        if (v_x2 < hjust_range || v_x2 > (1 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=v_x2, xend=v_x2, y=0, yend=v_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%.2f", v_x2), x=v_x2, y=v_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    }
  }

  if (n_graph > 1) {
    graph <- cowplot::plot_grid(plotlist=graphs, ncol=ncol, nrow=nrow, scale=0.9)
  }

  return(graph)
})


#' @title compare_distributions
#' @description \code{compare_distributions} draws samples from distribution of the first group and compares them against samples drawn from the distribution of the second group or against a color defined with rgb or hsv components. You can also provide the rope parameter or execute the comparison only through chosen color components (r, g, b, h, s, v).
#' @param object color_class object.
#' @param ... fit2 - a second color_class object, rgb - color defined through rgb, hsv - color defined through rgb, rope - region of practical equivalence, pars - components of comparison, a subset of (r, g, b, h, s, v).
#' @rdname color_class-compare_distributions
#' @aliases compare_distributions_color
#' @return Comparison results or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="compare_distributions", signature(object="color_class"), definition=function(object, ...) {
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the compare_distributions function are invalid, compare_distributions(color_class, fit2=color_class), compare_distributions(color_class, rgb=vector) or compare_distributions(color_class, hsv=vector) is required! You can optionallly provide the rope parameter, e.g. compare_distributions(color_class, fit2=color_class, rope=numeric). You can also execute the comparison through a subset of color components, e.g. compare_distributions(color_class, fit2=color_class, pars=c(\"h\", \"s\", \"v\"))."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # comparing with another fit, rgb or hsv
  fit2 <- NULL
  rgb <- NULL
  hsv <- NULL

  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "color_class") {
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
  } else if (!is.null(arguments$rgb)) {
    rgb <- arguments$rgb
    hsv <- grDevices::rgb2hsv(rgb)
  } else if (!is.null(arguments$hsv)) {
    hsv <- arguments$hsv
    rgb <- hsv2rgb(hsv[1], hsv[2], hsv[3])
  }

  # are all null?
  if (is.null(fit2) && is.null(rgb) && is.null(hsv)) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # compare through all components or through a subset
  pars <- c("r", "g", "b", "h", "s", "v")
  if (!is.null(arguments$pars)) {
    pars <- arguments$pars
  }

  # compare
  result <- NULL
  n <- 100000
  for (p in pars) {
    if (p == "r") {
      mu1 <- mean(object@extract$mu_r)
      sigma1 <- mean(object@extract$sigma_r)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_r)
        sigma2 <- mean(fit2@extract$sigma_r)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- rgb[1]
      }

      cat("\n------------- R component -------------\n")
      r <- difference(y1=y1, y2=y2, rope=rope, max_diff=255)
      result <- rbind(result, r)
    } else if (p == "g") {
      mu1 <- mean(object@extract$mu_g)
      sigma1 <- mean(object@extract$sigma_g)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_g)
        sigma2 <- mean(fit2@extract$sigma_g)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- rgb[2]
      }

      cat("\n------------- G component -------------\n")
      g <- difference(y1=y1, y2=y2, rope=rope, max_diff=255)
      result <- rbind(result, g)
    } else if (p == "b") {
      mu1 <- mean(object@extract$mu_b)
      sigma1 <- mean(object@extract$sigma_b)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_b)
        sigma2 <- mean(fit2@extract$sigma_b)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- rgb[3]
      }

      cat("\n------------- B component -------------\n")
      b <- difference(y1=y1, y2=y2, rope=rope, max_diff=255)
      result <- rbind(result, b)
    } else if (p == "h") {
      mu1 <- mean(preprocess_circular((object@extract$mu_h)))
      kappa1 <- mean(object@extract$kappa_h)

      suppressWarnings(y1 <- circular::rvonmises(n, mu=mu1, kappa=kappa1))

      if (!is.null(fit2)) {
        mu2 <- mean(preprocess_circular(fit2@extract$mu_h, base=object@extract$mu_h))
        kappa2 <- mean(fit2@extract$kappa_h)

        suppressWarnings(y2 <- circular::rvonmises(n, mu=mu2, kappa=kappa2))
      } else {
        y2 <- hsv[1]
      }

      cat("\n------------- H component -------------\n")
      h <- circular_difference(y1=y1, y2=y2, rope=rope)
      result <- rbind(result, h)
    } else if (p == "s") {
      mu1 <- mean(object@extract$mu_s)
      sigma1 <- mean(object@extract$sigma_s)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_s)
        sigma2 <- mean(fit2@extract$sigma_s)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- hsv[2]
      }

      cat("\n------------- S component -------------\n")
      s <- difference(y1=y1, y2=y2, rope=rope, max_diff=1)
      result <- rbind(result, s)
    } else if (p == "v") {
      mu1 <- mean(object@extract$mu_v)
      sigma1 <- mean(object@extract$sigma_v)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_v)
        sigma2 <- mean(fit2@extract$sigma_v)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- hsv[3]
      }

      cat("\n------------- V component -------------\n")
      v <- difference(y1=y1, y2=y2, rope=rope, max_diff=1)
      result <- rbind(result, v)
    }
  }

  cat("\n")
  return(result)
})


#' @title plot_distributions
#' @description \code{plot_distributions} a visualization of the fitted distributions or constant colors.
#' @param object color_class object.
#' @param ... fit2 - a second color_class object, rgb - color defined through rgb, hsv - color defined through rgb, pars - components of comparison, a subset of (r, g, b, h, s, v).
#' @rdname color_class-plot_distributions
#' @aliases plot_distributions_color
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_distributions", signature(object="color_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  value <- x <- y <- NULL

  # get arguments
  arguments <- NULL
  if (length(list(...))) {
    arguments <- list(...)
  }

  # comparing with another fit, rgb or hsv
  fit2 <- NULL
  rgb <- NULL
  hsv <- NULL

  if (!is.null(arguments) && (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "color_class")) {
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
  } else if (!is.null(arguments$rgb)) {
    rgb <- arguments$rgb
    hsv <- grDevices::rgb2hsv(rgb)
  } else if (!is.null(arguments$hsv)) {
    hsv <- arguments$hsv
    rgb <- hsv2rgb(hsv[1], hsv[2], hsv[3])
  }

  # plot all components or a subset
  pars <- c("r", "g", "b", "h", "s", "v")
  if (!is.null(arguments$pars)) {
    pars <- arguments$pars
  }

  # calculate number of columns and rows
  n_graph <- length(pars)
  nrow <- 1
  ncol <- 1
  if (n_graph > 1) {
    nrow <- ceiling(n_graph / 3)
    ncol <- 3
    if (n_graph == 2 || n_graph == 4) {
      ncol <- 2
    }
  }

  n <- 1000

  # plot
  graphs <- list()
  i <- 1
  for (p in pars) {
    if (p == "r") {
      # first group data
      r_mu1 <- mean(object@extract$mu_r)
      r_sigma1 <- mean(object@extract$sigma_r)

      # plot
      df_x <- data.frame(value=c(0, 255))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=r_mu1, sd=r_sigma1), geom="area", fill="#808080", alpha=0.5) +
        xlab("value") +
        ggtitle("r") +
        theme(plot.title=element_text(hjust=0.5))

      if (!is.null(fit2)) {
        # second group data
        r_mu2 <- mean(fit2@extract$mu_r)
        r_sigma2 <- mean(fit2@extract$sigma_r)

        graph <- graph + stat_function(fun=stats::dnorm, n=n, args=list(mean=r_mu2, sd=r_sigma2), geom="line", color="#000000", size=1)
      } else if (!is.null(rgb)) {
        # predefined color
        r_x2 <- rgb[1]
        r_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 255 * 0.1
        hjust = "center"
        if (r_x2 < hjust_range || r_x2 > (255 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=r_x2, xend=r_x2, y=0, yend=r_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%d", r_x2), x=r_x2, y=r_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "g") {
      # first group data
      g_mu1 <- mean(object@extract$mu_g)
      g_sigma1 <- mean(object@extract$sigma_g)

      # plot
      df_x <- data.frame(value=c(0, 255))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=g_mu1, sd=g_sigma1), geom="area", fill="#808080", alpha=0.5) +
        xlab("value") +
        ggtitle("g") +
        theme(plot.title=element_text(hjust=0.5))

      if (!is.null(fit2)) {
        # second group data
        g_mu2 <- mean(fit2@extract$mu_g)
        g_sigma2 <- mean(fit2@extract$sigma_g)

        graph <- graph + stat_function(fun=stats::dnorm, n=n, args=list(mean=g_mu2, sd=g_sigma2), geom="line", color="#000000", size=1)
      } else if (!is.null(rgb)) {
        # predefined color
        g_x2 <- rgb[2]
        g_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 255 * 0.1
        hjust = "center"
        if (g_x2 < hjust_range || g_x2 > (255 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=g_x2, xend=g_x2, y=0, yend=g_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%d", g_x2), x=g_x2, y=g_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "b") {
      # first group data
      b_mu1 <- mean(object@extract$mu_b)
      b_sigma1 <- mean(object@extract$sigma_b)

      # plot
      df_x <- data.frame(value=c(0, 255))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=b_mu1, sd=b_sigma1), geom="area", fill="#808080", alpha=0.5) +
        xlab("value") +
        ggtitle("b") +
        theme(plot.title=element_text(hjust=0.5))

      if (!is.null(fit2)) {
        # second group data
        b_mu2 <- mean(fit2@extract$mu_b)
        b_sigma2 <- mean(fit2@extract$sigma_b)

        graph <- graph + stat_function(fun=stats::dnorm, n=n, args=list(mean=b_mu2, sd=b_sigma2), geom="line", color="#000000", size=1)
      } else if (!is.null(rgb)) {
        # predefined color
        b_x2 <- rgb[3]
        b_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 255 * 0.1
        hjust = "center"
        if (b_x2 < hjust_range || b_x2 > (255 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=b_x2, xend=b_x2, y=0, yend=b_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%d", b_x2), x=b_x2, y=b_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "h") {
      # first group data
      h_mu1 <- mean(preprocess_circular(object@extract$mu_h))
      h_kappa1 <- mean(object@extract$kappa_h)

      # plot on -pi..pi or 0..2pi
      if (h_mu1 < pi/2 & h_mu1 > -pi/2) {
        x_min <- -pi
        x_max <- pi
      } else if (h_mu1 < -pi/2) {
        x_min <- -2*pi
        x_max <- 0
      } else {
        x_min <- 0
        x_max <- 2*pi
      }

      # suppress circular warnings
      h_mu1 <- suppressWarnings(circular::as.circular(h_mu1))
      h_kappa1 <- suppressWarnings(circular::as.circular(h_kappa1))

      # get data points
      step <- 2*pi/n
      h_x <- suppressWarnings(circular::as.circular(seq(x_min, x_max, step)))
      h_y1 <- circular::dvonmises(h_x, h_mu1, h_kappa1)
      df1 <- data.frame(x=h_x, y=h_y1)

      # plot
      graph <- ggplot() +
        geom_area(data=df1, aes(x=x, y=y), fill="#808080", alpha=0.5) +
        xlab("value") +
        ggtitle("h") +
        theme(plot.title=element_text(hjust=0.5))

      if (!is.null(fit2)) {
        # second group data
        h_mu2 <- mean(preprocess_circular(fit2@extract$mu_h, base=object@extract$mu_h))
        h_kappa2 <- mean(fit2@extract$kappa_h)

        # suppress circular warnings
        h_mu2 <- suppressWarnings(circular::as.circular(h_mu2))
        h_kappa2 <- suppressWarnings(circular::as.circular(h_kappa2))

        # get data points
        h_y2 <- circular::dvonmises(h_x, h_mu2, h_kappa2)
        df2 <- data.frame(x=h_x, y=h_y2)

        # plot
        graph <- graph + geom_line(data=df2, aes(x=x, y=y), color="#000000", size=1)
      } else if (!is.null(hsv)) {
        # predefined color
        h_x2 <- hsv[1]
        h_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 2*pi * 0.1
        hjust = "center"
        if (h_x2 < (x_min + hjust_range) || h_x2 > (x_max - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=h_x2, xend=h_x2, y=0, yend=h_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%.2f", h_x2), x=h_x2, y=h_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "s") {
      # first group data
      s_mu1 <- mean(object@extract$mu_s)
      s_sigma1 <- mean(object@extract$sigma_s)

      # plot
      df_x <- data.frame(value=c(0, 1))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=s_mu1, sd=s_sigma1), geom="area", fill="#808080", alpha=0.5) +
        xlab("value") +
        ggtitle("s") +
        theme(plot.title=element_text(hjust=0.5))

      if (!is.null(fit2)) {
        # second group data
        s_mu2 <- mean(fit2@extract$mu_s)
        s_sigma2 <- mean(fit2@extract$sigma_s)

        graph <- graph + stat_function(fun=stats::dnorm, n=n, args=list(mean=s_mu2, sd=s_sigma2), geom="line", color="#000000", size=1)
      } else if (!is.null(hsv)) {
        # predefined color
        s_x2 <- hsv[2]
        s_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 1 * 0.1
        hjust = "center"
        if (s_x2 < hjust_range || s_x2 > (1 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=s_x2, xend=s_x2, y=0, yend=s_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%.2f", s_x2), x=s_x2, y=s_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "v") {
      # first group data
      v_mu1 <- mean(object@extract$mu_v)
      v_sigma1 <- mean(object@extract$sigma_v)

      # plot
      df_x <- data.frame(value=c(0, 1))

      # plot
      graph <- ggplot(data=df_x, aes(x=value)) +
        stat_function(fun=stats::dnorm, n=n, args=list(mean=v_mu1, sd=v_sigma1), geom="area", fill="#808080", alpha=0.5) +
        xlab("value") +
        ggtitle("v") +
        theme(plot.title=element_text(hjust=0.5))

      if (!is.null(fit2)) {
        # second group data
        v_mu2 <- mean(fit2@extract$mu_v)
        v_sigma2 <- mean(fit2@extract$sigma_v)

        graph <- graph + stat_function(fun=stats::dnorm, n=n, args=list(mean=v_mu2, sd=v_sigma2), geom="line", color="#000000", size=1)
      } else if (!is.null(hsv)) {
        # predefined color
        v_x2 <- hsv[3]
        v_y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

        hjust_range <- 1 * 0.1
        hjust = "center"
        if (v_x2 < hjust_range || v_x2 > (1 - hjust_range)) {
          hjust = "inward"
        }

        graph <- graph +
          geom_segment(aes(x=v_x2, xend=v_x2, y=0, yend=v_y_max[2] * 1.05), size=1, color="#000000", na.rm=T) +
          geom_text(aes(label=sprintf("%.2f", v_x2), x=v_x2, y=v_y_max[2] * (1.05 + (nrow * 0.05))), size=4, hjust=hjust)
      }

      graphs[[i]] <- graph
      i <- i + 1
    }
  }

  if (n_graph > 1) {
    graph <- cowplot::plot_grid(plotlist=graphs, ncol=ncol, nrow=nrow, scale=0.9)
  }

  return(graph)
})


#' @title plot_distributions_difference
#' @description \code{plot_distributions_difference} a visualization of the difference between the distribution of the first group and the second group.
#' @param object color_class object.
#' @param ... fit2 - a second color_class object, rgb - color defined through rgb, hsv - color defined through rgb, rope - region of practical equivalence, bins - number of bins in the histogram, pars - components of comparison, a subset of (r, g, b, h, s, v).
#' @rdname color_class-plot_distributions_difference
#' @aliases plot_distributions_difference_color
#' @return A ggplot visualization or an error if something went wrong.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_distributions_difference", signature(object="color_class"), definition=function(object, ...) {
  # get arguments
  arguments <- list(...)

  wrong_arguments <- "The provided arguments for the plot_distributions_difference function are invalid, plot_distributions_difference(color_class, fit2=color_class), plot_distributions_difference(color_class, rgb=vector) or plot_distributions_difference(color_class, hsv=vector) is required! You can optionallly provide the rope parameter, e.g. plot_distributions_difference(color_class, fit2=color_class, rope=numeric) or the bins parameter plot_distributions_difference(color_class, fit2=color_class, bins=numeric). You can also execute the comparison through a subset of color components, e.g. plot_distributions_difference(color_class, fit2=color_class, pars=c(\"h\", \"s\", \"v\"))."

  if (length(arguments) == 0) {
    stop(wrong_arguments)
  }

  # comparing with another fit, rgb or hsv
  fit2 <- NULL
  rgb <- NULL
  hsv <- NULL

  if (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "color_class") {
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }
  } else if (!is.null(arguments$rgb)) {
    rgb <- arguments$rgb
    hsv <- grDevices::rgb2hsv(rgb)
  } else if (!is.null(arguments$hsv)) {
    hsv <- arguments$hsv
    rgb <- hsv2rgb(hsv[1], hsv[2], hsv[3])
  }

  # are all null?
  if (is.null(fit2) && is.null(rgb) && is.null(hsv)) {
    stop(wrong_arguments)
  }

  # prepare rope
  rope <- NULL
  if (!is.null(arguments$rope)) {
    rope <- arguments$rope
  }
  rope <- prepare_rope(rope)

  # bins in the histogram
  bins <- 30
  if (!is.null(arguments$bins)) {
    bins <- arguments$bins
  }

  # compare through all components or through a subset
  pars <- c("r", "g", "b", "h", "s", "v")
  if (!is.null(arguments$pars)) {
    pars <- arguments$pars
  }

  # calculate number of columns and rows
  n_graph <- length(pars)
  nrow <- 1
  ncol <- 1
  if (n_graph > 1) {
    nrow <- ceiling(n_graph / 3)
    ncol <- 3
    if (n_graph == 2 || n_graph == 4) {
      ncol <- 2
    }
  }

  # plot
  n <- 100000
  graphs <- list()
  i <- 1
  for (p in pars) {
    if (p == "r") {
      mu1 <- mean(object@extract$mu_r)
      sigma1 <- mean(object@extract$sigma_r)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_r)
        sigma2 <- mean(fit2@extract$sigma_r)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- rgb[1]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow, max_diff=255)
      graph <- graph + ggtitle("r") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "g") {
      mu1 <- mean(object@extract$mu_g)
      sigma1 <- mean(object@extract$sigma_g)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_g)
        sigma2 <- mean(fit2@extract$sigma_g)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- rgb[2]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow, max_diff=255)
      graph <- graph + ggtitle("g") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "b") {
      mu1 <- mean(object@extract$mu_b)
      sigma1 <- mean(object@extract$sigma_b)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_b)
        sigma2 <- mean(fit2@extract$sigma_b)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- rgb[3]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow, max_diff=255)
      graph <- graph + ggtitle("b") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "h") {
      mu1 <- mean(preprocess_circular((object@extract$mu_h)))
      kappa1 <- mean(object@extract$kappa_h)

      suppressWarnings(y1 <- circular::rvonmises(n, mu=mu1, kappa=kappa1))

      if (!is.null(fit2)) {
        mu2 <- mean(preprocess_circular(fit2@extract$mu_h, base=object@extract$mu_h))
        kappa2 <- mean(fit2@extract$kappa_h)

        suppressWarnings(y2 <- circular::rvonmises(n, mu=mu2, kappa=kappa2))
      } else {
        y2 <- hsv[1]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, circular=TRUE, nrow=nrow)
      graph <- graph + ggtitle("h") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "s") {
      mu1 <- mean(object@extract$mu_s)
      sigma1 <- mean(object@extract$sigma_s)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_s)
        sigma2 <- mean(fit2@extract$sigma_s)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- hsv[2]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow, max_diff=1)
      graph <- graph + ggtitle("s") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    } else if (p == "v") {
      mu1 <- mean(object@extract$mu_v)
      sigma1 <- mean(object@extract$sigma_v)

      y1 <- stats::rnorm(n, mean=mu1, sd=sigma1)

      if (!is.null(fit2)) {
        mu2 <- mean(fit2@extract$mu_v)
        sigma2 <- mean(fit2@extract$sigma_v)

        y2 <- stats::rnorm(n, mean=mu2, sd=sigma2)
      } else {
        y2 <- hsv[3]
      }

      graph <- plot_difference(y1=y1, y2=y2, rope=rope, bins=bins, nrow=nrow, max_diff=1)
      graph <- graph + ggtitle("v") + theme(plot.title=element_text(hjust=0.5))
      graphs[[i]] <- graph
      i <- i + 1
    }
  }

  if (n_graph > 1) {
    graph <- cowplot::plot_grid(plotlist=graphs, ncol=ncol, nrow=nrow, scale=0.9)
  }

  return(graph)
})


#' @rdname color_class-plot_fit_hsv
#' @exportMethod plot_fit_hsv
setGeneric(name="plot_fit_hsv", function(object) standardGeneric("plot_fit_hsv"))

#' @title plot_fit_hsv
#' @description \code{plot_fit_hsv} plots fitted model against the data. Use this function to explore the quality of your fit thorough a circular visualization of hsv color components.
#' @param object color_class object.
#' @rdname color_class-plot_fit_hsv
#' @aliases plot_fit_hsv_color
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_fit_hsv", signature(object="color_class"), definition=function(object) {
  # init local varibales for CRAN check
  r <- g <- b <- h <- s <- v <- sv <- NULL

  df_colors <- data.frame(r=object@data$r,
                          g=object@data$g,
                          b=object@data$b,
                          h=object@data$h,
                          s=object@data$s,
                          v=object@data$v)

  # in order to present this in a joined colourwheel, recode s and v to a joint variable
  df_colors <- df_colors %>% mutate(sv=ifelse(s == 1, v - 1, (s - 1) * -1))

  # cast h to 0..1
  df_colors$h <- df_colors$h / (2*pi)

  # annotate mu
  mu_h_processed <- preprocess_circular(object@extract$mu_h)
  h <- mean(mu_h_processed)
  if (h < 0) {
    h <- h + 2 * pi
    mu_h_processed <- mu_h_processed + 2 * pi
  }

  # annotate HDI
  kappa <- mean(object@extract$kappa_h)
  # suppress circular warnings
  h_mean <- suppressWarnings(circular::as.circular(h))
  kappa <- suppressWarnings(circular::as.circular(kappa))

  # draw from distribution
  n <- 10000
  y <- circular::rvonmises(n, h_mean, kappa)
  y <- preprocess_circular(y, base=h_mean)
  h_hdi <- as.numeric(mcmc_hdi(y))

  # cast to 0..1
  h_hdi <- h_hdi / (2*pi)
  h_low <- h_hdi[1]
  h_high <- h_hdi[2]

  if (h_low < 0 && h_high < 0) {
    h_low <- h_low + 1
    h_high <- h_high + 1
  } else if (h_low > 1 && h_high > 1) {
    h_low <- h_low - 1
    h_high <- h_high - 1
  } else if (h_low < 0) {
    h_low <- h_low + 1
    h_low <- c(h_low, 0)
    h_high <- c(1, h_high)
  } else if (h_high > 1) {
    h_high <- h_high - 1
    h_low <- c(h_low, 0)
    h_high <- c(1, h_high)
  }

  # create data frame
  h <- h / (2*pi)
  r <- mean(object@extract$mu_r)
  g <- mean(object@extract$mu_g)
  b <- mean(object@extract$mu_b)
  df_fit <- data.frame(h=h, r=r, g=g, b=b)

  graph <- ggplot() +
    annotate(geom="rect", xmin=0, xmax=1, ymin=-1, ymax=1, alpha=0.1) +
    geom_hline(yintercept=0, color="gray20", size=1) +
    geom_point(data=df_colors, aes(x=h, y=sv, color=grDevices::rgb(r, g, b, maxColorValue=255)), size=5, alpha=0.5, shape=16) +
    annotate(geom="rect", xmin=h_low, xmax=h_high, ymin=-1, ymax=1, fill=grDevices::rgb(r, g, b, maxColorValue=255), alpha=0.5) +
    geom_segment(data=df_fit, aes(x=h, xend=h, y=-1, yend=1, color=grDevices::rgb(r, g, b, maxColorValue=255)), size=2) +
    scale_colour_identity() +
    theme(axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          axis.line=element_blank(),
          panel.grid=element_blank()) +
    coord_polar() +
    scale_y_continuous(limits=c(-3, 1), expand=c(0,0)) +
    scale_x_continuous(limits=c(0, 1), expand=c(0,0))

  return(graph)
})



#' @rdname color_class-plot_hsv
#' @exportMethod plot_hsv
setGeneric(name="plot_hsv", function(object) standardGeneric("plot_hsv"))

#' @title plot_hsv
#' @description \code{plot_hsv} plots fitted model against the data. Use this function to explore the quality of your fit thorough a circular visualization of hsv color components.
#' @param object color_class object.
#' @rdname color_class-plot_hsv
#' @aliases plot_hsv_color
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_hsv", signature(object="color_class"), definition=function(object) {
  return(plot_fit_hsv(object))
})


#' @rdname color_class-plot_means_hsv
#' @exportMethod plot_means_hsv
setGeneric(name="plot_means_hsv", function(object, ...) standardGeneric("plot_means_hsv"))

#' @title plot_means_hsv
#' @description \code{plot_means_hsv} a visualization of the difference between means of two fits through a circular visualization of hsv color components. You can also compare fit means with colors defined through rgb or hsv components (as points or as lines on the visualization).
#' @param object color_class object.
#' @param ... fit2 - a second color_class object, points - points to plot defined through rgb or hsv, lines - lines to plot defined through rgb or hsv, hsv - are points and lines defined in hsv format (default = FALSE).
#' @rdname color_class-plot_means_hsv
#' @aliases plot_means_hsv_color
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_means_hsv", signature(object="color_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  points <- lines <- r <- g <- b <- h <- s <- v <- sv <- NULL

  # get arguments
  arguments <- list(...)

  # are provided colors hsv or rgb
  hsv <- FALSE
  if (length(arguments) != 0 && !is.null(arguments$hsv)) {
    hsv <- TRUE
  }

  # start graph
  graph <- ggplot() +
    annotate(geom="rect", xmin=0, xmax=1, ymin=-1, ymax=1, alpha=0.1) +
    geom_hline(yintercept=0, color="gray20", size=1)

  # did user provide custom points
  if (length(arguments) != 0 && !is.null(arguments$points)) {
    points <- arguments$points

    df_points <- data.frame(r=numeric(),
                            g=numeric(),
                            b=numeric(),
                            h=numeric(),
                            s=numeric(),
                            v=numeric())

    for (p in points) {
      if (hsv) {
        h <- p[1]
        if (h < 0)
          h <- h + 2*pi
        s <- p[2]
        v <- p[3]
        rgb <- hsv2rgb(h, s, v)
        r <- rgb[1]
        g <- rgb[2]
        b <- rgb[3]
        df_points <- rbind(df_points, data.frame(r=r, g=g, b=b, h=h, s=s, v=v))
      } else {
        r <- p[1]
        g <- p[2]
        b <- p[3]
        hsv <- grDevices::rgb2hsv(p)
        h <- hsv[1]
        s <- hsv[2]
        v <- hsv[3]
        df_points <- rbind(df_points, data.frame(r=r, g=g, b=b, h=h, s=s, v=v))
      }
    }

    # in order to present this in a joined colourwheel, recode s and v to a joint variable
    df_points <- df_points %>% mutate(sv=ifelse(s == 1, v - 1, (s - 1) * -1))

    # cast h to 0..1
    df_points$h <- df_points$h / (2*pi)

    # add to graph
    graph <- graph +
      geom_point(data=df_points, aes(x=h, y=sv, color=grDevices::rgb(r, g, b, maxColorValue=255)), size=5, shape=16)
  }

  # annotate mu
  mu_h_processed <- preprocess_circular(object@extract$mu_h)
  h <- mean(mu_h_processed)
  if (h < 0) {
    h <- h + 2 * pi
    mu_h_processed <- mu_h_processed + 2 * pi
  }
  h <- h / (2*pi)
  r <- mean(object@extract$mu_r)
  g <- mean(object@extract$mu_g)
  b <- mean(object@extract$mu_b)

  # annotate HDI
  h_hdi <- mcmc_hdi(mu_h_processed)

  # cast to 0..1
  h_hdi <- h_hdi / (2*pi)
  h_low <- h_hdi[1]
  h_high <- h_hdi[2]

  if (h_low < 0 && h_high < 0) {
    h_low <- h_low + 1
    h_high <- h_high + 1
  } else if (h_low > 1 && h_high > 1) {
    h_low <- h_low - 1
    h_high <- h_high - 1
  } else if (h_low < 0) {
    h_low <- h_low + 1
    h_low <- c(h_low, 0)
    h_high <- c(1, h_high)
  } else if (h_high > 1) {
    h_high <- h_high - 1
    h_low <- c(h_low, 0)
    h_high <- c(1, h_high)
  }

  # fits
  df_fits <- data.frame(r=numeric(),
                        g=numeric(),
                        b=numeric(),
                        h=numeric())

  df_fits <- rbind(df_fits, data.frame(h=h, r=r, g=g, b=b))

  graph <- graph +
    annotate(geom="rect", xmin=h_low, xmax=h_high, ymin=-1, ymax=1, fill=grDevices::rgb(r, g, b, maxColorValue=255), alpha=0.5)

  # fit2 provided?
  if (length(arguments) != 0 && (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "color_class")) {
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }

    # annotate mu
    mu_h2_processed <- preprocess_circular(fit2@extract$mu_h)
    h_2 <- mean(mu_h2_processed)
    if (h_2 < 0) {
      h_2 <- h + 2 * pi
      mu_h2_processed <- mu_h2_processed + 2 * pi
    }
    h_2 <- h_2 / (2*pi)
    r_2 <- mean(fit2@extract$mu_r)
    g_2 <- mean(fit2@extract$mu_g)
    b_2 <- mean(fit2@extract$mu_b)

    df_fits <- rbind(df_fits, data.frame(h=h_2, r=r_2, g=g_2, b=b_2))

    # annotate HDI
    h_hdi_2 <- mcmc_hdi(mu_h2_processed)

    # cast to 0..1
    h_hdi_2 <- h_hdi_2 / (2*pi)
    h_low_2 <- h_hdi_2[1]
    h_high_2 <- h_hdi_2[2]

    if (h_low_2 < 0) {
      h_low_2 <- h_low_2 + 1
      h_low_2 <- c(h_low_2, 0)
      h_high_2 <- c(1, h_high_2)
    } else if (h_high_2 > 1) {
      h_high_2 <- h_high_2 - 1
      h_low_2 <- c(h_low_2, 0)
      h_high_2 <- c(1, h_high_2)
    }

    graph <- graph +
      annotate(geom="rect", xmin=h_low_2, xmax=h_high_2, ymin=-1, ymax=1, fill=grDevices::rgb(r_2, g_2, b_2, maxColorValue=255), alpha=0.5)
  }


  # did user provide custom lines
  if (length(arguments) != 0 && !is.null(arguments$lines)) {
    lines <- arguments$lines

    df_lines <- data.frame(r=numeric(),
                           g=numeric(),
                           b=numeric(),
                           h=numeric(),
                           s=numeric(),
                           v=numeric(),
                           id=factor())

    id = 1
    for (l in lines) {
      if (hsv) {
        h <- l[1]
        if (h < 0)
          h <- h + 2*pi
        s <- l[2]
        v <- l[3]
        rgb <- hsv2rgb(h, s, v)
        r <- rgb[1]
        g <- rgb[2]
        b <- rgb[3]
        df_lines <- rbind(df_lines, data.frame(r=r, g=g, b=b, h=h, s=s, v=v, id=as.factor(id)))
      } else {
        r <- l[1]
        g <- l[2]
        b <- l[3]
        hsv <- grDevices::rgb2hsv(l)
        h <- hsv[1]
        s <- hsv[2]
        v <- hsv[3]
        df_lines <- rbind(df_lines, data.frame(r=r, g=g, b=b, h=h, s=s, v=v, id=as.factor(id)))
      }
      id <- id + 1
    }

    # cast h to 0..1
    df_lines$h <- df_lines$h / (2*pi)

    # add to graph
    graph <- graph +
      geom_segment(data=df_lines, aes(x=h, xend=h, y=-3, yend=1, color=grDevices::rgb(r, g, b, maxColorValue=255), linetype=id), size=2)
  }

  # add fits and beutify chart
  graph <- graph +
    geom_segment(data=df_fits, aes(x=h, xend=h, y=-1, yend=1, color=grDevices::rgb(r, g, b, maxColorValue=255)), size=2) +
    scale_colour_identity() +
    theme(axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          axis.line=element_blank(),
          panel.grid=element_blank(),
          legend.position="none") +
    coord_polar() +
    scale_y_continuous(limits=c(-3, 1), expand=c(0,0)) +
    scale_x_continuous(limits=c(0, 1), expand=c(0,0))

  return(graph)
})


#' @rdname color_class-plot_distributions_hsv
#' @exportMethod plot_distributions_hsv
setGeneric(name="plot_distributions_hsv", function(object, ...) standardGeneric("plot_distributions_hsv"))

#' @title plot_distributions_hsv
#' @description \code{plot_distributions_hsv} a visualization of distributions of one or two fits thorough a circular visualization of hsv color components. You can also compare fit means with colors defined through rgb or hsv components (as points or as lines on the visualization).
#' @param object color_class object.
#' @param ... fit2 - a second color_class object, points - points to plot defined through rgb or hsv, lines - lines to plot defined through rgb or hsv, hsv - are points and lines defined in hsv format (default = FALSE).
#' @rdname color_class-plot_distributions_hsv
#' @aliases plot_distributions_hsv_color
#' @return A ggplot visualization.
#'
#' @examples
#' # to use the function you first have to prepare the data and fit the model
#' # see class documentation for an example of the whole process
#' # along with an example of how to use this function
#' ?color_class
#'
setMethod(f="plot_distributions_hsv", signature(object="color_class"), definition=function(object, ...) {
  # init local varibales for CRAN check
  points <- lines <- r <- g <- b <- h <- s <- v <- sv <- NULL

  # get arguments
  arguments <- list(...)

  # are provided colors hsv or rgb
  hsv <- FALSE
  if (length(arguments) != 0 && !is.null(arguments$hsv)) {
    hsv <- TRUE
  }

  # start graph
  graph <- ggplot() +
    annotate(geom="rect", xmin=0, xmax=1, ymin=-1, ymax=1, alpha=0.1) +
    geom_hline(yintercept=0, color="gray20", size=1)

  # did user provide custom points
  if (length(arguments) != 0 && !is.null(arguments$points)) {
    points <- arguments$points

    df_points <- data.frame(r=numeric(),
                            g=numeric(),
                            b=numeric(),
                            h=numeric(),
                            s=numeric(),
                            v=numeric())

    for (p in points) {
      if (hsv) {
        h <- p[1]
        if (h < 0)
          h <- h + 2*pi
        s <- p[2]
        v <- p[3]
        rgb <- hsv2rgb(h, s, v)
        r <- rgb[1]
        g <- rgb[2]
        b <- rgb[3]
        df_points <- rbind(df_points, data.frame(r=r, g=g, b=b, h=h, s=s, v=v))
      } else {
        r <- p[1]
        g <- p[2]
        b <- p[3]
        hsv <- grDevices::rgb2hsv(p)
        h <- hsv[1]
        s <- hsv[2]
        v <- hsv[3]
        df_points <- rbind(df_points, data.frame(r=r, g=g, b=b, h=h, s=s, v=v))
      }
    }

    # in order to present this in a joined colourwheel, recode s and v to a joint variable
    df_points <- df_points %>% mutate(sv=ifelse(s == 1, v - 1, (s - 1) * -1))

    # cast h to 0..1
    df_points$h <- df_points$h / (2*pi)

    # add to graph
    graph <- graph +
      geom_point(data=df_points, aes(x=h, y=sv, color=grDevices::rgb(r, g, b, maxColorValue=255)), size=5, shape=16)
  }

  # annotate mu
  h <- mean(preprocess_circular(object@extract$mu_h))
  if (h < 0) {
    h <- h + 2 * pi
  }
  r <- mean(object@extract$mu_r)
  g <- mean(object@extract$mu_g)
  b <- mean(object@extract$mu_b)

  # annotate HDI
  kappa <- mean(object@extract$kappa_h)
  # suppress circular warnings
  h <- suppressWarnings(circular::as.circular(h))
  kappa <- suppressWarnings(circular::as.circular(kappa))

  # draw from distribution
  n <- 10000
  y <- circular::rvonmises(n, h, kappa)
  y <- preprocess_circular(y, base=h)
  h_hdi <- as.numeric(mcmc_hdi(y))

  # cast to 0..1
  h_hdi <- h_hdi / (2*pi)
  h_low <- h_hdi[1]
  h_high <- h_hdi[2]

  if (h_low < 0 && h_high < 0) {
    h_low <- h_low + 1
    h_high <- h_high + 1
  } else if (h_low > 1 && h_high > 1) {
    h_low <- h_low - 1
    h_high <- h_high - 1
  } else if (h_low < 0) {
    h_low <- h_low + 1
    h_low <- c(h_low, 0)
    h_high <- c(1, h_high)
  } else if (h_high > 1) {
    h_high <- h_high - 1
    h_low <- c(h_low, 0)
    h_high <- c(1, h_high)
  }

  # fits
  h <- h / (2*pi)
  df_fits <- data.frame(r=numeric(),
                        g=numeric(),
                        b=numeric(),
                        h=numeric())

  df_fits <- rbind(df_fits, data.frame(h=h, r=r, g=g, b=b))

  graph <- graph +
    annotate(geom="rect", xmin=h_low, xmax=h_high, ymin=-1, ymax=1, fill=grDevices::rgb(r, g, b, maxColorValue=255), alpha=0.5)

  # fit2 provided?
  if (length(arguments) != 0 && (!is.null(arguments$fit2) || class(arguments[[1]])[1] == "color_class")) {
    if (!is.null(arguments$fit2)) {
      fit2 <- arguments$fit2
    } else {
      fit2 <- arguments[[1]]
    }

    # annotate mu
    h_2 <- mean(preprocess_circular(fit2@extract$mu_h))
    if (h_2 < 0) {
      h_2 <- h + 2 * pi
    }
    r_2 <- mean(fit2@extract$mu_r)
    g_2 <- mean(fit2@extract$mu_g)
    b_2 <- mean(fit2@extract$mu_b)

    # annotate HDI
    kappa_2 <- mean(fit2@extract$kappa_h)
    # suppress circular warnings
    h_2 <- suppressWarnings(circular::as.circular(h_2))
    kappa_2 <- suppressWarnings(circular::as.circular(kappa_2))

    # draw from distribution
    y_2 <- circular::rvonmises(n, h_2, kappa_2)
    y_2 <- preprocess_circular(y_2, base=h_2)
    h_hdi_2 <- as.numeric(mcmc_hdi(y_2))

    # cast to 0..1
    h_hdi_2 <- h_hdi_2 / (2*pi)
    h_low_2 <- h_hdi_2[1]
    h_high_2 <- h_hdi_2[2]

    if (h_low_2 < 0) {
      h_low_2 <- h_low_2 + 1
      h_low_2 <- c(h_low_2, 0)
      h_high_2 <- c(1, h_high_2)
    } else if (h_high_2 > 1) {
      h_high_2 <- h_high_2 - 1
      h_low_2 <- c(h_low_2, 0)
      h_high_2 <- c(1, h_high_2)
    }

    # fits
    h_2 <- h_2 / (2*pi)
    df_fits <- rbind(df_fits, data.frame(h=h_2, r=r_2, g=g_2, b=b_2))

    graph <- graph +
      annotate(geom="rect", xmin=h_low_2, xmax=h_high_2, ymin=-1, ymax=1, fill=grDevices::rgb(r_2, g_2, b_2, maxColorValue=255), alpha=0.5)
  }

  # did user provide custom lines
  if (length(arguments) != 0 && !is.null(arguments$lines)) {
    lines <- arguments$lines

    df_lines <- data.frame(r=numeric(),
                           g=numeric(),
                           b=numeric(),
                           h=numeric(),
                           s=numeric(),
                           v=numeric(),
                           id=factor())

    id = 1
    for (l in lines) {
      if (hsv) {
        h <- l[1]
        if (h < 0)
          h <- h + 2*pi
        s <- l[2]
        v <- l[3]
        rgb <- hsv2rgb(h, s, v)
        r <- rgb[1]
        g <- rgb[2]
        b <- rgb[3]
        df_lines <- rbind(df_lines, data.frame(r=r, g=g, b=b, h=h, s=s, v=v, id=as.factor(id)))
      } else {
        r <- l[1]
        g <- l[2]
        b <- l[3]
        hsv <- grDevices::rgb2hsv(l)
        h <- hsv[1]
        s <- hsv[2]
        v <- hsv[3]
        df_lines <- rbind(df_lines, data.frame(r=r, g=g, b=b, h=h, s=s, v=v, id=as.factor(id)))
      }
      id <- id + 1
    }

    # cast h to 0..1
    df_lines$h <- df_lines$h / (2*pi)

    # add to graph
    graph <- graph +
      geom_segment(data=df_lines, aes(x=h, xend=h, y=-3, yend=1, color=grDevices::rgb(r, g, b, maxColorValue=255), linetype=id), size=2)
  }

  # add fits and beutify chart
  graph <- graph +
    geom_segment(data=df_fits, aes(x=h, xend=h, y=-1, yend=1, color=grDevices::rgb(r, g, b, maxColorValue=255)), size=2) +
    scale_colour_identity() +
    theme(axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          axis.line=element_blank(),
          panel.grid=element_blank(),
          legend.position="none") +
    coord_polar() +
    scale_y_continuous(limits=c(-3, 1), expand=c(0,0)) +
    scale_x_continuous(limits=c(0, 1), expand=c(0,0))

  return(graph)
})
