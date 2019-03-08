#' @title b_success_rate
#' @description Bayesian model for comparing test success rate.
#' @import rstan
#' @export
#' @param r a vector containting test results (0 - test was not solved successfully, 1 - test was solved succesfully).
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @param warmup Integer specifying the number of warmup iterations per chain.
#' @param iter Integer specifying the number of iterations (including warmup).
#' @return An object of class `success_rate_class`.
b_success_rate <- function(r, s, warmup=200, iter=1200) {

  n <- length(r)
  m <- length(unique(s))

  stan_data <- list(n = n,
                    m = m,
                    r = r,
                    s = s)

  fit <- sampling(stanmodels$success_rate,
                         data = stan_data,
                         iter = iter,
                         warmup = warmup,
                         chains = 1,
                         control = list(adapt_delta = 0.99, max_treedepth = 15))

  extract <- extract(fit)

  # create output class
  out <- success_rate_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
