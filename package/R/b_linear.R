#' @title b_linear
#' @description Bayesian model for fitting a linear normal model to data.
#' @import rstan
#' @export
#' @param x a vector containting index of sequence (time).
#' @param y a vector containting subjet's responses.
#' @param s a vector contaiting subject indexes. Starting index should be 1, and the largest subject index equals m (number of subjects).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 2000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 3000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `linear_class`.
b_linear <- function(x, y, s, warmup=2000, iter=3000, chains=4, control=NULL) {

  n <- length(y)
  m <- length(unique(s))

  stan_data <- list(n=n,
                    m=m,
                    x=x,
                    y=y,
                    s=s)

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
