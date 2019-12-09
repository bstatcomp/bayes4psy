#' @title b_linear
#' @description Bayesian model for fitting a linear normal model to data.
#' @import rstan
#' @export
#' @param x a vector containing sequence indexes (time).
#' @param y a vector containing responses of subjects.
#' @param s a vector containing subject indexes. Starting index should be 1 and the largest subject index should equal the number of subjects.
#' @param priors List of parameters and their priors - b_prior objects. You can put a prior on the mu_a (mean intercept), sigma_a (variance of mu_a), mu_b (mean slope), sigma_s (variance of mu_b), mu_s (variance) and sigma_s (variance of mu_s) parameters (default = NULL).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 1000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 2000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param seed Random number generator seed (default = NULL).
#' @param refresh Frequency of output (default = NULL).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @param suppress_warnings Suppress warnings returned by Stan (default = TRUE).
#' @return An object of class `linear_class`.
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
#' # generate data
#' x <- vector()
#' y <- vector()
#' s <- vector()
#' for (i in 1:5) {
#'   x <- c(x, rep(1:10, 2))
#'   y <- c(y, rnorm(20, mean=1:10, sd=2))
#'   s <- c(s, rep(i, 20))
#' }
#'
# fit
#' fit <- b_linear(x=x, y=y, s=s, priors=priors, chains=1)
#' }
#'
b_linear <- function(x,
                     y,
                     s,
                     priors=NULL,
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
  n <- length(y)
  m <- length(unique(s))

  # prior ids and values
  p_ids <- rep(0, 6)
  p_values <- rep(0, 12)

  # parameter mapping
  df_pars <- data.frame(par=c("mu_a", "sigma_a", "mu_b", "sigma_b", "mu_s", "sigma_s"), index=seq(1, 6))

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
                    m=m,
                    x=x,
                    y=y,
                    s=s,
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
    fit <- suppressWarnings(sampling(stanmodels$linear,
                                     data=stan_data,
                                     iter=iter,
                                     warmup=warmup,
                                     chains=chains,
                                     seed=seed,
                                     refresh=refresh,
                                     control=control))
  } else {
    fit <- sampling(stanmodels$linear,
                    data=stan_data,
                    iter=iter,
                    warmup=warmup,
                    chains=chains,
                    seed=seed,
                    refresh=refresh,
                    control=control)
  }

  # extract and parse into list
  extract_raw <- extract(fit, permuted=FALSE)

  # alpha
  i <- 1
  j <- m
  alpha <- extract_raw[, 1, i:j]

  # beta
  i <- i + m
  j <- i + m - 1
  beta <- extract_raw[, 1, i:j]

  # sigma
  i <- i + m
  j <- i + m - 1
  sigma <- extract_raw[, 1, i:j]

  # mu_a
  i <- i + m
  mu_a <- extract_raw[, 1, i]

  # mu_b
  i <- i + 1
  mu_b <- extract_raw[, 1, i]

  # mu_s
  i <- i + 1
  mu_s <- extract_raw[, 1, i]

  # sigma_a
  i <- i + 1
  sigma_a <- extract_raw[, 1, i]

  # sigma_b
  i <- i + 1
  sigma_b <- extract_raw[, 1, i]

  # sigma_s
  i <- i + 1
  sigma_s <- extract_raw[, 1, i]

  # lp__
  i <- i + 1
  lp__ <- extract_raw[, 1, i]

  extract <- list(alpha=alpha,
                  beta=beta,
                  sigma=sigma,
                  mu_a=mu_a,
                  mu_b=mu_b,
                  mu_s=mu_s,
                  sigma_a=sigma_a,
                  sigma_b=sigma_b,
                  sigma_s=sigma_s,
                  lp__=lp__)

  # create output class
  out <- linear_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
