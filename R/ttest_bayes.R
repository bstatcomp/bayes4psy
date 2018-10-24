#' Bayesian t-test.
#'
#' @export
#' @param y1 Numeric vector of values for the first group.
#' @param y2 Optional parameter for values for the second group.
#' @param mu Optional parameter for comparison of first group against a mean value.
#' @param sigma Optional parameter for comparison of first group against a distribution N(mu, sigma).
#' @param rope Optional parameter for region of practical equivalence, default = NULL
#' @return An object of class `ttest_bayes_results`.
#'
ttest_bayes <- function(y1,
                        y2 = NULL,
                        mu = NULL,
                        sigma = NULL,
                        rope = NULL) {

  # fit
  fit <- ttest_fit(y1, y2)
  extract <- rstan::extract(fit)

  # create output class
  out <- ttest_results(extract = extract, mu = mu, sigma = sigma, rope = rope)

  # return
  return(out)
}
