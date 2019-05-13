#' @title b_ttest
#' @description Bayesian t-test.
#' @import rstan
#' @export
#' @param data Numeric vector of values on which the fit will be based.
#' @param p_mu Prior for the mu (mean value) parameter (default = NULL).
#' @param p_sigma Prior for the sigma (variance) parameter (default = NULL).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 9000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 10000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `ttest_class`.
b_ttest <- function(data, p_mu=NULL, p_sigma=NULL, warmup=9000, iter=10000, chains=4, control=NULL) {

  # prepare data
  n <- length(data)

  # priors
  # prior id (1 = uniform, 2 = normal, 3 = gamma, 4 = beta)
  p_mu_id <- 0
  # prior parameters
  p_mu1 <- 0
  p_mu2 <- 0
  if (!is.null(p_mu)) {
    if (p_mu@family == "uniform") {
      p_mu_id <- 1
    } else if (p_mu@family == "normal") {
      p_mu_id <- 2
    } else if (p_mu@family == "gamma") {
      p_mu_id <- 3
    } else if (p_mu@family == "beta") {
      p_mu_id <- 4
    } else {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_mu@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_mu=b_prior(family=\"normal\", pars=c(100, 20))."
      warning(wrong_pars)
      return()
    }
    p_mu1 <- p_mu@pars[1]
    p_mu2 <- p_mu@pars[2]
  }

  # prior id (1 = uniform, 2 = normal, 3 = gamma, 4 = beta)
  p_sigma_id <- 0
  # prior parameters
  p_sigma1 <- 0
  p_sigma2 <- 0
  if (!is.null(p_sigma)) {
    if (p_sigma@family == "uniform") {
      p_sigma_id <- 1
    } else if (p_sigma@family == "normal") {
      p_sigma_id <- 2
    } else if (p_sigma@family == "gamma") {
      p_sigma_id <- 3
    } else if (p_sigma@family == "beta") {
      p_sigma_id <- 4
    } else {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_sigma@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_mu=b_prior(family=\"normal\", pars=c(100, 20))."
      warning(wrong_pars)
      return()
    }
    p_sigma1 <- p_sigma@pars[1]
    p_sigma2 <- p_sigma@pars[2]
  }

  stan_data <- list(n = n,
                    y = data,
                    p_mu = p_mu_id,
                    p_mu1 = p_mu1,
                    p_mu2 = p_mu2,
                    p_sigma= p_sigma_id,
                    p_sigma1 = p_sigma1,
                    p_sigma2 = p_sigma2)

  fit <- sampling(stanmodels$ttest,
                  data = stan_data,
                  warmup = warmup,
                  iter = iter,
                  chains = chains,
                  control=control)

  extract <- extract(fit)

  # create output class
  out <- ttest_class(extract=extract, fit=fit, data=data)

  # return
  return(out)
}
