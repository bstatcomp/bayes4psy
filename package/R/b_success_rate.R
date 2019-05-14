#' @title b_success_rate
#' @description Bayesian model for comparing test success rate.
#' @import rstan
#' @export
#' @param r a vector containting test results (0 - test was not solved successfully, 1 - test was solved succesfully).
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @param p_p Prior for the p (mean value) parameter (default = NULL/flat improper prior).
#' @param p_tau Prior for the tau (variance) parameter (default = NULL/flat improper prior).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 2000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 3000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `success_rate_class`.
b_success_rate <- function(r,
                           s,
                           p_p=NULL,
                           p_tau=NULL,
                           warmup=2000,
                           iter=3000,
                           chains=4,
                           control=NULL) {

  # prepare data
  n <- length(r)
  m <- length(unique(s))

  # prior ids and values
  p_ids <- rep(0, 2)
  p_values <- rep(0, 4)

  # p
  id <- 1
  if (!is.null(p_p)) {
    p_ids[id] <- get_prior_id(p_p)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_p@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_p=b_prior(family=\"beta\", pars=c(1, 1))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_p@pars[1]
    p_values[2*id] <- p_p@pars[2]
  }

  # tau
  id <- 2
  if (!is.null(p_tau)) {
    p_ids[id] <- get_prior_id(p_tau)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_tau@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_tau=b_prior(family=\"uniform\", pars=c(0, 0.2))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_tau@pars[1]
    p_values[2*id] <- p_tau@pars[2]
  }

  stan_data <- list(n=n,
                    m=m,
                    r=r,
                    s=s,
                    p_ids = p_ids,
                    p_values = p_values)

  fit <- sampling(stanmodels$success_rate,
                  data=stan_data,
                  iter=iter,
                  warmup=warmup,
                  chains = chains,
                  control=control)

  extract <- extract(fit)

  # create output class
  out <- success_rate_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
