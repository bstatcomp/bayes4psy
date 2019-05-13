#' @title b_reaction_time
#' @description Bayesian model for comparing reaction times.
#' @import rstan
#' @export
#' @param rt a vector containing reaction times for each measurement.
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 2000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 3000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `reaction_time_class`.
b_reaction_time <- function(t, s, warmup=2000, iter=3000, chains=4, control=NULL) {

  n <- length(t)
  m <- length(unique(s))

  stan_data <- list(n=n,
                    m=m,
                    t=t,
                    s=s)

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
