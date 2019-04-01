#' @title b_linear
#' @description Bayesian model for fitting a linear normal model to data.
#' @import rstan
#' @export
#' @param x a vector containting index of sequence (time).
#' @param y a vector containting subjet's responses.
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 200).
#' @param iter Integer specifying the number of iterations (including warmup, default = 1200).
#' @return An object of class `linear_class`.
b_linear <- function(x, y, s, warmup=200, iter=1200) {

  n <- length(y)
  m <- length(unique(s))

  stan_data <- list(n = n,
                    m = m,
                    x = x,
                    y = y,
                    s = s)

  fit <- sampling(stanmodels$linear,
                         data = stan_data,
                         iter = iter,
                         warmup = warmup,
                         chains = 1,
                         control = list(adapt_delta=0.99, max_treedepth=15))

  extract <- extract(fit)

  # create output class
  out <- linear_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
