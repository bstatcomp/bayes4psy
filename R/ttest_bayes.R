#' Bayesian t-test.
#'
#' @export
#' @param y1 Numeric vector of values for the first group.
#' @param y2 Numeric vector of values for the second group.
#' @param rope Optional parameter for region of practical equivalence, default = NULL.
#' @return An object of class `ttest_bayes_results`.
#'
ttest_bayes <- function(y1,
                        y2,
                        rope = 0) {

  # fit normal distribution to first group
  fit1 <- ttest_fit(y1)
  extract1 <- rstan::extract(fit1)

  # fit normal distribution to second group
  fit2 <- ttest_fit(y2)
  extract2 <- rstan::extract(fit2)

  # create output class
  out <- ttest_results(y1_samples = extract1, y2_samples = extract2, rope = rope)

  # return
  return(out)
}
