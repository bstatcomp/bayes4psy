#' @title b_ttest
#' @description Bayesian t-test.
#' @import rstan
#' @export
#' @param y Numeric vector of values on which the fit will be based.
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 9000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 10000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `ttest_class`.
b_ttest <- function(y, warmup=9000, iter=10000, chains=4, control=NULL) {

  # prepare data
  n <- length(y)

  # used for priors
  yMu <- mean(y)
  ySd <- stats::sd(y)

  stan_data <- list(n = n,
                    y = y,
                    yMu = yMu,
                    ySd = ySd)

  fit <- sampling(stanmodels$ttest,
                  data = stan_data,
                  warmup = warmup,
                  iter = iter,
                  chains = chains,
                  control=control)

  extract <- extract(fit)

  # create output class
  out <- ttest_class(extract=extract, fit=fit, data=y)

  # return
  return(out)
}
