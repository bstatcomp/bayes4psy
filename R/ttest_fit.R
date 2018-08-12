#' Bayesian Student's t-test model fitting.
#'
#' @param y Numeric vector of values.
#' @return An object of class `stanfit` returned by `rstan::sampling`.
#'
ttest_fit <- function(y) {
  yMu <- mean(y)
  ySd <- sd(y) * 1000
  uL <- sd(y) / 1000
  uH <- sd(y) * 1000

  stan_data <- list(
    n = length(y),
    y = y,
    yMu = yMu,
    ySd = ySd
  )

  out <- rstan::sampling(stanmodels$ttest,
    data = stan_data,
    chains = 1,
    warmup = 12000,
    iter = 15000
  )

  return(out)
}
