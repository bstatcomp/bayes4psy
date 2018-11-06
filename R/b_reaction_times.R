#' Bayesian model for comparing reaction times and test success rate.
#'
#' @export
#' @param n total number of measurements.
#' @param m number of participating subjects.
#' @param rt a vector containing reaction times for each measurement.
#' @param r a vector containting test results (0 - test was not solved successfully, 1 - test was solved succesfully).
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @return An object of class `rt_results_class`.
#'
b_reaction_times <- function(n,
                    m,
                    rt,
                    r,
                    s) {


  stan_data <- list(n = n,
                    m = m,
                    rt = rt,
                    r = r,
                    s = s)

  fit <- rstan::sampling(stanmodels$reaction_times,
                          data = stan_data,
                          iter = 1000,
                          warmup = 800,
                          chains = 1,
                          control = list(adapt_delta = 0.99, max_treedepth = 15))

  extract <- rstan::extract(fit)

  # create output class
  out <- reaction_times_results(extract = extract, data = stan_data)

  # return
  return(out)
}
