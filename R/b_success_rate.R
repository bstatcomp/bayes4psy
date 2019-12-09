#' @title b_success_rate
#' @description Bayesian model for comparing test success rate.
#' @import rstan
#' @export
#' @param r a vector containing test results (0 - test was not solved successfully, 1 - test was solved successfully).
#' @param s a vector containing subject indexes. Starting index should be 1 and the largest subject index should equal the number of subjects.
#' @param priors List of parameters and their priors - b_prior objects. You can put a prior on the p (mean probability of success) and tau (variance) parameters (default = NULL).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 1000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 2000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param seed Random number generator seed (default = NULL).
#' @param refresh Frequency of output (default = NULL).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @param suppress_warnings Suppress warnings returned by Stan (default = TRUE).
#' @return An object of class `success_rate_class`.
#'
#' @examples
#' \donttest{
#' # priors
#' p_prior <- b_prior(family="beta", pars=c(1, 1))
#' tau_prior <- b_prior(family="uniform", pars=c(0, 500))
#'
#' # attach priors to relevant parameters
#' priors <- list(c("p", p_prior),
#'                c("tau", tau_prior))
#'
#' # generate data
#' s <- rep(1:5, 20)
#' data <- rbinom(100, size=1, prob=0.6)
#'
#' # fit
#' fit <- b_success_rate(r=data, s=s, priors=priors, chains=1)
#' }
#'
b_success_rate <- function(r,
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
  n <- length(r)
  m <- length(unique(s))

  # prior ids and values
  p_ids <- rep(0, 2)
  p_values <- rep(0, 4)

  # parameter mapping
  df_pars <- data.frame(par=c("p", "tau"), index=c(1, 2))

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
        wrong_prior <- "Provided an unknown parameter for prior, use \"p\" or \"tau\"."
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
                    r=r,
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
    fit <- suppressWarnings(sampling(stanmodels$success_rate,
                                     data=stan_data,
                                     iter=iter,
                                     warmup=warmup,
                                     chains=chains,
                                     seed=seed,
                                     refresh=refresh,
                                     control=control))
  } else {
    fit <- sampling(stanmodels$success_rate,
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

  # p0
  i <- 1
  p0 <- extract_raw[, 1, i]

  # tau
  i <- i + 1
  tau <- extract_raw[, 1, i]

  # p
  i <- i + 1
  j <- i + m - 1
  p <- extract_raw[, 1, i:j]

  # lp__
  i <- i + m
  lp__ <- extract_raw[, 1, i]

  extract <- list(p0=p0,
                  tau=tau,
                  p=p,
                  lp__=lp__)

  # create output class
  out <- success_rate_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
