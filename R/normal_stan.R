#' Bayesian normal model
#'
#' @export
#' @param y Numeric vector of values.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
normal_stan <- function(y,
                        yMu = 0,
                        ySd = 1,
                        uL = 0,
                        uH = 2,
                        warmup = 1000,
                        iter = 1500,
                        seed = sample.int(.Machine$integer.max, 1)) {

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
    warmup = warmup,
    iter = iter,
    seed = seed
  )

  return(out)
}
