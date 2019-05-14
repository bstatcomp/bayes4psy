#' @title b_ttest
#' @description Bayesian t-test.
#' @import rstan
#' @export
#' @param data Numeric vector of values on which the fit will be based.
#' @param p_mu Prior for the mu (mean value) parameter (default = NULL/flat improper prior).
#' @param p_sigma Prior for the sigma (variance) parameter (default = NULL/flat improper prior).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 9000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 10000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `ttest_class`.
b_ttest <- function(data,
                    p_mu=NULL,
                    p_sigma=NULL,
                    warmup=9000,
                    iter=10000,
                    chains=4,
                    control=NULL) {

  # prepare data
  n <- length(data)

  # prior ids and values
  p_ids <- rep(0, 2)
  p_values <- rep(0, 4)

  # mu
  id <- 1
  if (!is.null(p_mu)) {
    p_ids[id] <- get_prior_id(p_mu)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_mu@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_mu=b_prior(family=\"normal\", pars=c(100, 20))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_mu@pars[1]
    p_values[2*id] <- p_mu@pars[2]
  }

  # sigma
  id <- 2
  if (!is.null(p_sigma)) {
    p_ids[id] <- get_prior_id(p_sigma)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_sigma@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_sigma=b_prior(family=\"uniform\", pars=c(0, 100))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_sigma@pars[1]
    p_values[2*id] <- p_sigma@pars[2]
  }

  stan_data <- list(n = n,
                    y = data,
                    p_ids = p_ids,
                    p_values = p_values)

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
