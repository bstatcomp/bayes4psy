#' Bayesian normal model
#'
#' @param y Numeric vector of values.
#' @param log_skewed Optional parameter for determing if data is log skewed, default = FALSE.
#' @param warmup Optional parameter for number of burnin, warmup steps, default = 1000.
#' @param iter Optional parameter for number of iterations (warmup + samples), default = 1500.
#' @param seed Optional parameter for determinng random seed used in Stan MCMC, default = sample.int(.Machine$integer.max, 1)
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
normal_stan <- function(y, log_skewed = FALSE) {
  yMu = mean(y)
  ySd = sd(y) * 1000
  uL = sd(y) / 1000
  uH = sd(y) * 1000

  stan_data <- list(
    n = length(y),
    y = y,
    yMu = yMu,
    ySd = ySd,
    uL = uL,
    uH = uH,
    log_skewed = FALSE
  )

  out <- rstan::sampling(stanmodels$normal,
    data = stan_data,
    chains = 1,
    warmup = 12000,
    iter = 15000
  )

  return(out)
}
