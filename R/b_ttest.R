#' Bayesian t-test.
#'
#' @export
#' @param y Numeric vector of values on which the fit will be based.
#' @return An object of class `ttest_results_class`.
#'
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

  fit <- rstan::sampling(stanmodels$ttest,
                         data = stan_data,
                         chains = 1,
                         warmup = 12000,
                         iter = 15000)

  extract <- rstan::extract(fit)

  # create output class
  out <- ttest_results(extract = extract, fit = fit, data = y)

  # return
  return(out)
}
