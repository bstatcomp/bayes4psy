#' @title b_reaction_time
#' @description Bayesian model for comparing reaction times.
#' @import rstan
#' @export
#' @param t a vector containing reaction times for each measurement.
#' @param s a vector containing subject indexes. Starting index should be 1 and the largest subject index should equal the number of subjects.
#' @param priors List of parameters and their priors - b_prior objects. You can put a prior on the mu_m (mean), sigma_m (variance of mu_m), mu_s (variance), sigma_s (variance of mu_s), mu_l (mean of the exponent factor) and sigma_l (variance of mu_l) parameters (default = NULL).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 1000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 2000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param seed Random number generator seed (default = NULL).
#' @param refresh Frequency of output (default = NULL).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @param suppress_warnings Suppress warnings returned by Stan (default = TRUE).
#' @return An object of class `reaction_time_class`
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
#'               c("sigma_m", sigma_prior),
#'               c("mu_s", sigma_prior),
#'               c("sigma_s", sigma_prior),
#'               c("mu_l", lambda_prior),
#'               c("sigma_l", sigma_prior))
#'
#' # generate data
#' s <- rep(1:5, 20)
#' rt <- emg::remg(100, mu=10, sigma=1, lambda=0.4)
#'
#' # fit
#' fit <- b_reaction_time(t=rt, s=s, priors=priors, chains=1)
#' }
#'
b_reaction_time <- function(t,
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
  n <- length(t)
  m <- length(unique(s))

  # prior ids and values
  p_ids <- rep(0, 6)
  p_values <- rep(0, 12)

  # parameter mapping
  df_pars <- data.frame(par=c("mu_m", "sigma_m", "mu_s", "sigma_s", "mu_l", "sigma_l"), index=seq(1, 6))

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
                    t=t,
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
    fit <- suppressWarnings(sampling(stanmodels$reaction_time,
                                     data=stan_data,
                                     iter=iter,
                                     warmup=warmup,
                                     chains=chains,
                                     seed=seed,
                                     refresh=refresh,
                                     control=control))
  } else {
    fit <- sampling(stanmodels$reaction_time,
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

  # mu
  i <- 1
  j <- m
  mu <- extract_raw[, 1, i:j]

  # sigma
  i <- i + m
  j <- i + m - 1
  sigma <- extract_raw[, 1, i:j]

  # lambda
  i <- i + m
  j <- i + m - 1
  lambda <- extract_raw[, 1, i:j]

  # mu_m
  i <- i + m
  mu_m <- extract_raw[, 1, i]

  # mu_l
  i <- i + 1
  mu_l <- extract_raw[, 1, i]

  # mu_s
  i <- i + 1
  mu_s <- extract_raw[, 1, i]

  # sigma_m
  i <- i + 1
  sigma_m <- extract_raw[, 1, i]

  # sigma_l
  i <- i + 1
  sigma_l <- extract_raw[, 1, i]

  # sigma_s
  i <- i + 1
  sigma_s <- extract_raw[, 1, i]

  # rt
  i <- i + 1
  rt <- extract_raw[, 1, i]

  # rt_subjects
  i <- i + 1
  j <- i + m - 1
  rt_subjects <- extract_raw[, 1, i:j]

  # lp__
  i <- i + m
  lp__ <- extract_raw[, 1, i]

  extract <- list(mu=mu,
                  sigma=sigma,
                  lambda=lambda,
                  mu_m=mu_m,
                  mu_l=mu_l,
                  mu_s=mu_s,
                  sigma_m=sigma_m,
                  sigma_l=sigma_l,
                  sigma_s=sigma_s,
                  rt=rt,
                  rt_subjects=rt_subjects,
                  lp__=lp__)

  # create output class
  out <- reaction_time_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
