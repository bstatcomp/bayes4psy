#' @title b_color
#' @description Bayesian model for comparing colors.
#' @import rstan
#' @export
#' @param colors a data frame of colors either in RGB or HSV format. The first column should be the R (or H) component, the second column should be the G (or S) component and the third column should be the B (or V) component.
#' @param hsv set to TRUE if colors are provided in HSV format (default = FALSE).
#' @param priors List of parameters and their priors - b_prior objects. You can put a prior on the mu_r (mean r component), sigma_r (variance of mu_r), mu_g (mean g component), sigma_g (variance of mu_g), mu_b (mean b component), sigma_b (variance of mu_b), mu_h (mean h component), kappa_h (variance of mu_h), mu_s (mean s component), sigma_s (variance of mu_s), mu_v (mean v component) and sigma_v (variance of mu_v) parameters (default = NULL).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 1000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 2000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param seed Random number generator seed (default = NULL).
#' @param refresh Frequency of output (default = NULL).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @param suppress_warnings Suppress warnings returned by Stan (default = TRUE).
#' @return An object of class `color_class`
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
#' # generate data (rgb)
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
#' colors_rgb <- data.frame(r=r, g=g, b=b)
#'
#' # fit
#' fit_rgb <- b_color(colors=colors_rgb, priors=priors_rgb, chains=1)
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
#' # generate data (hsv)
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
#' colors_hsv <- data.frame(h=h, s=s, v=v)
#'
#' # fit
#' fit_hsv <- b_color(colors=colors_hsv, hsv=TRUE, priors=priors_hsv, chains=1)
#' }
#'
b_color <- function(colors,
                    priors=NULL,
                    hsv=FALSE,
                    warmup=1000,
                    iter=2000,
                    chains=4,
                    seed=NULL,
                    refresh=NULL,
                    control=NULL,
                    suppress_warnings=TRUE) {

  # multi core
  if (chains > 1) {
    options(mc.cores = parallel::detectCores())
  }

  # prepare data
  n <- nrow(colors)

  if (!hsv) {
    # extract rgb
    r <- colors[,1]
    g <- colors[,2]
    b <- colors[,3]

    # cast to hsv
    colors[c("h", "s", "v")] <- with(colors, t(rgb2hsv(r, g, b, maxColorValue=255)))
    colors$h <- colors$h * 2 * pi
    h <- colors$h
    s <- colors$s
    v <- colors$v
  } else {
    # extract hsv
    h <- colors[,1]
    s <- colors[,2]
    v <- colors[,3]

    # cast to rgb
    colors[c("r", "g", "b")] <- with(colors, t(hsv2rgb(h, s, v)))
    r <- colors$r
    g <- colors$g
    b <- colors$b
  }

  # prior ids and values
  p_ids <- rep(0, 12)
  p_values <- rep(0, 24)

  # parameter mapping
  df_pars <- data.frame(par=c("mu_r", "sigma_r",
                              "mu_g", "sigma_g",
                              "mu_b", "sigma_b",
                              "mu_h", "kappa_h",
                              "mu_s", "sigma_s",
                              "mu_v", "sigma_v"),
                        index=seq(1, 12))

  # priors
  if (!is.null(priors)) {
    for (p in priors) {
      par <- p[[1]]
      prior <- p[[2]]

      # get parameter index
      id <- 0
      par_id <- df_pars[df_pars$par==par,]
      if (nrow(par_id) > 0) {
        id <- par_id$index
      } else {
        wrong_prior <- "Provided an unknown parameter for prior, use \"mu_m\", \"sigma_m\", \"mu_s\", \"sigma_s\", \"mu_l\" or \"sigma_l\"."
        warning(wrong_prior)
        return()
      }
      # set prior family id
      p_ids[id] <- get_prior_id(prior)
      if (p_ids[id] == 0) {
        wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
        warning(wrong_prior)
        return()
      }

      # set parameter values
      if (length(prior@pars) != 2) {
        wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values."
        warning(wrong_pars)
        return()
      }
      p_values[2*id-1] <- prior@pars[1]
      p_values[2*id] <- prior@pars[2]
    }
  }

  # put data together
  stan_data <- list(n=n,
                    r=r,
                    g=g,
                    b=b,
                    h=h,
                    s=s,
                    v=v,
                    p_ids = p_ids,
                    p_values = p_values)

  # set seed
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  # set output frequency
  if (is.null(refresh)) {
    refresh <- max(iter/10, 1)
  }

  # fit
  if (suppress_warnings) {
    fit <- suppressWarnings(sampling(stanmodels$color,
                                     data=stan_data,
                                     iter=iter,
                                     warmup=warmup,
                                     chains=chains,
                                     seed=seed,
                                     refresh=refresh,
                                     control=control))
  } else {
    fit <- sampling(stanmodels$color,
                    data=stan_data,
                    iter=iter,
                    warmup=warmup,
                    chains=chains,
                    seed=seed,
                    refresh=refresh,
                    control=control)
  }

  # extract and parse into data frame
  extract_raw <- extract(fit, permuted=FALSE)
  extract <- NULL
  samples <- iter - warmup
  for (i in 1:samples) {
    extract <- rbind(extract, extract_raw[i, 1,])
  }
  extract <- as.list(data.frame(extract))

  # create output class
  out <- color_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
