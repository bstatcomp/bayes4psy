#' @title b_reaction_time
#' @description Bayesian model for comparing reaction times.
#' @import rstan
#' @export
#' @param t a vector containing reaction times for each measurement.
#' @param s a vector contaiting subject indexes. Starting index should be 1 and the largest subject index equals m (number of subjects).
#' @param priors List of parameters and their priors - b_prior objects. You can put a prior on the mu_m (mean), sigma_m (variance of mu_m), mu_s (variance), sigma_s (variance of mu_s), mu_l (mean of the exponent factor) and sigma_l (variance of mu_l) parameters (default = NULL).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 2000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 3000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `reaction_time_class`.
b_reaction_time <- function(t,
                            s,
                            priors=NULL,
                            warmup=2000,
                            iter=3000,
                            chains=4,
                            control=NULL) {

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

  stan_data <- list(n=n,
                    m=m,
                    t=t,
                    s=s,
                    p_ids = p_ids,
                    p_values = p_values)

  fit <- sampling(stanmodels$reaction_time,
                  data=stan_data,
                  iter=iter,
                  warmup=warmup,
                  chains=chains,
                  control=control)

  extract <- extract(fit)

  # create output class
  out <- reaction_time_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
