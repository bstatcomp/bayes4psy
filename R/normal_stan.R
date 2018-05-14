#' Bayesian normal model
#'
#' @export
#' @param x Numeric vector of input values.
#' @param y Numberic vector of output values.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
normal_stan <- function(y, ...) {
  standata <- list(n = length(y), y = y, yMu = 0, ySd = 1, uL = 0, uH = 2, log_skewed = FALSE)
  out <- rstan::sampling(stanmodels$normal, data = standata, ...)
  return(out)
}
