#' @title b_ttest
#' @description Bayesian t-test.
#' @import rstan stats
#' @export
#' @param y Numeric vector of values on which the fit will be based.
#' @return An object of class `ttest_class`.
b_ttest <- function(y) {

  # prepare data
  n <- length(y)

  # used for priors
  yMu <- mean(y)
  ySd <- sd(y)

  stan_data <- list(
    n = n,
    y = y,
    yMu = yMu,
    ySd = ySd
  )

  fit <- sampling(stanmodels$ttest,
                         data = stan_data,
                         chains = 1,
                         warmup = 9000,
                         iter = 10000)

  extract <- extract(fit)

  # create output class
  out <- ttest_class(extract = extract, fit = fit, data = y)

  # return
  return(out)
}
