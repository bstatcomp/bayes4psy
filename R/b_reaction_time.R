#' Bayesian model for comparing reaction times and test success rate.
#'
#' @export
#' @param n total number of measurements.
#' @param m number of participating subjects.
#' @param rt a vector containing reaction times for each measurement.
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @return An object of class `reaction_time_class`.
#'
b_reaction_time <- function(n,
                    m,
                    rt,
                    s) {

  stan_data <- list(n = n,
                    m = m,
                    rt = rt,
                    s = s)

  fit <- rstan::sampling(stanmodels$reaction_times,
                        data = stan_data,
                        iter = 400,
                        warmup = 200,
                        chains = 1,
                        control = list(adapt_delta = 0.99, max_treedepth = 15))

  extract <- rstan::extract(fit)

  # create output class
  out <- reaction_time_class(extract = extract, data = stan_data, fit = fit)

  # return
  return(out)
}
