#' @title b_linear
#' @description Bayesian model for fitting a linear normal model to data.
#' @export
#' @param n total number of measurements.
#' @param m number of participating subjects.
#' @param x a vector containting index of sequence (time).
#' @param y a vector containting subjet's responses.
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @return An object of class `linear_class`.
b_linear <- function(n, m, x, y, s) {

  stan_data <- list(n = n,
                    m = m,
                    x = x,
                    y = y,
                    s = s)

  fit <- rstan::sampling(stanmodels$linear,
                         data = stan_data,
                         iter = 1200,
                         warmup = 200,
                         chains = 1,
                         control = list(adapt_delta = 0.99, max_treedepth = 15))

  extract <- rstan::extract(fit)

  # create output class
  out <- linear_class(extract = extract, data = stan_data, fit = fit)

  # return
  return(out)
}
