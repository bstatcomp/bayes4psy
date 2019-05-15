#' @title b_linear
#' @description Bayesian model for fitting a linear normal model to data.
#' @import rstan
#' @export
#' @param x a vector containting index of sequence (time).
#' @param y a vector containting subjet's responses.
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @param p_mu_a Prior for the mu_a (mean intercept) parameter (default = NULL/flat improper prior).
#' @param p_sigma_a Prior for the sigma_a (mu_a variance) parameter (default = NULL/flat improper prior).
#' @param p_mu_b Prior for the mu_b (mean slope) parameter (default = NULL/flat improper prior).
#' @param p_sigma_b Prior for the sigma_b (mu_b variance) parameter (default = NULL/flat improper prior).
#' @param p_mu_s Prior for the mu_s (variance) parameter (default = NULL/flat improper prior).
#' @param p_sigma_s Prior for the sigma_s (mu_s variance) parameter (default = NULL/flat improper prior).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 2000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 3000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `linear_class`.
b_linear <- function(x,
                     y,
                     s,
                     p_mu_a=NULL,
                     p_sigma_a=NULL,
                     p_mu_b=NULL,
                     p_sigma_b=NULL,
                     p_mu_s=NULL,
                     p_sigma_s=NULL,
                     warmup=2000,
                     iter=3000,
                     chains=4,
                     control=NULL) {

  # prepare data
  n <- length(y)
  m <- length(unique(s))

  # prior ids and values
  p_ids <- rep(0, 6)
  p_values <- rep(0, 12)

  # mu_m
  id <- 1
  if (!is.null(p_mu_a)) {
    p_ids[id] <- get_prior_id(p_mu_a)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_mu_a@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_mu_a=b_prior(family=\"normal\", pars=c(100, 20))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_mu_a@pars[1]
    p_values[2*id] <- p_mu_a@pars[2]
  }
  # sigma_m
  id <- 2
  if (!is.null(p_sigma_a)) {
    p_ids[id] <- get_prior_id(p_sigma_a)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_sigma_a@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_sigma_a=b_prior(family=\"uniform\", pars=c(0, 100))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_sigma_a@pars[1]
    p_values[2*id] <- p_sigma_a@pars[2]
  }

  # mu_s
  id <- 3
  if (!is.null(p_mu_b)) {
    p_ids[id] <- get_prior_id(p_mu_b)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_mu_b@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_mu_b=b_prior(family=\"normal\", pars=c(100, 20))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_mu_b@pars[1]
    p_values[2*id] <- p_mu_b@pars[2]
  }
  # sigma_s
  id <- 4
  if (!is.null(p_sigma_s)) {
    p_ids[id] <- get_prior_id(p_sigma_s)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_sigma_s@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_sigma_s=b_prior(family=\"uniform\", pars=c(0, 100))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_sigma_s@pars[1]
    p_values[2*id] <- p_sigma_s@pars[2]
  }

  # mu_l
  id <- 5
  if (!is.null(p_mu_s)) {
    p_ids[id] <- get_prior_id(p_mu_s)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_mu_s@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_mu_s=b_prior(family=\"normal\", pars=c(100, 20))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_mu_s@pars[1]
    p_values[2*id] <- p_mu_s@pars[2]
  }
  # sigma_l
  id <- 6
  if (!is.null(p_sigma_s)) {
    p_ids[id] <- get_prior_id(p_sigma_s)
    if (p_ids[id] == 0) {
      wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
      warning(wrong_prior)
      return()
    }

    if (length(p_sigma_s@pars) != 2) {
      wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values, e.g. p_sigma_s=b_prior(family=\"uniform\", pars=c(0, 100))."
      warning(wrong_pars)
      return()
    }
    p_values[2*id-1] <- p_sigma_s@pars[1]
    p_values[2*id] <- p_sigma_s@pars[2]
  }

  stan_data <- list(n=n,
                    m=m,
                    x=x,
                    y=y,
                    s=s,
                    p_ids = p_ids,
                    p_values = p_values)

  fit <- sampling(stanmodels$linear,
                  data=stan_data,
                  iter=iter,
                  warmup=warmup,
                  chains=chains,
                  control=control)

  extract <- extract(fit)

  # create output class
  out <- linear_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
