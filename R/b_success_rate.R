#' @title b_success_rate
#' @description Bayesian model for comparing reaction times and test success rate.
#' @export
#' @param n total number of measurements.
#' @param m number of participating subjects.
#' @param r a vector containting test results (0 - test was not solved successfully, 1 - test was solved succesfully).
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @return An object of class `success_rate_class`.
b_success_rate <- function(n,
                    m,
                    r,
                    s) {

  stan_data <- list(n = n,
                    m = m,
                    r = r,
                    s = s)

  fit <- rstan::sampling(stanmodels$success_rate,
                         data = stan_data,
                         iter = 1200,
                         warmup = 200,
                         chains = 1,
                         control = list(adapt_delta = 0.99, max_treedepth = 15))

  extract <- rstan::extract(fit)

  # create output class
  out <- success_rate_class(extract = extract, data = stan_data, fit = fit)

  # return
  return(out)
}
