#' @title b_reaction_time
#' @description Bayesian model for comparing reaction times.
#' @import rstan
#' @export
#' @param rt a vector containing reaction times for each measurement.
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @return An object of class `reaction_time_class`.
b_reaction_time <- function(rt, s) {

  n <- length(rt)
  m <- length(unique(s))

  stan_data <- list(n = n,
                    m = m,
                    rt = rt,
                    s = s)

  fit <- sampling(stanmodels$reaction_time,
                        data = stan_data,
                        iter = 1200,
                        warmup = 200,
                        chains = 1,
                        control = list(adapt_delta = 0.99, max_treedepth = 15))

  extract <- extract(fit)

  # create output class
  out <- reaction_time_class(extract = extract, data = stan_data, fit = fit)

  # return
  return(out)
}
