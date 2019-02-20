#' @title b_ttest
#' @description Bayesian t-test.
#' @import rstan
#' @export
#' @param y Numeric vector of values on which the fit will be based.
#' @param warmup Integer specifying the number of warmup iterations per chain.
#' @param iter Integer specifying the number of iterations (including warmup).
#' @return An object of class `ttest_class`.
b_ttest <- function(y, warmup=9000, iter=10000) {

  # prepare data
  n <- length(y)

  # used for priors
  yMu <- mean(y)
  ySd <- stats::sd(y)

  stan_data <- list(
    n = n,
    y = y,
    yMu = yMu,
    ySd = ySd
  )

  fit <- sampling(stanmodels$ttest,
                         data = stan_data,
                         chains = 1,
                         warmup = warmup,
                         iter = iter)

  extract <- extract(fit)

  # create output class
  out <- ttest_class(extract = extract, fit = fit, data = y)

  # return
  return(out)
}
