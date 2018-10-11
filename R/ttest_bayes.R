#' Bayesian t-test.
#'
#' @export
#' @param y1 Numeric vector of values for the first group.
#' @param y2 Optional parameter for values for the second group.
#' @param mu Optional parameter for comparison of first group against a mean value.
#' @param sigma Optional parameter for comparison of first group against a mean value and a variance.
#' @param rope Optional parameter for region of practical equivalence, default = NULL
#' @return An object of class `ttest_bayes_results`.
#'
ttest_bayes <- function(y1,
                        y2 = NULL,
                        mu = NULL,
                        sigma = NULL,
                        rope = NULL) {

  # fit normal distribution to first group
  fit1 <- ttest_fit(y1)
  extract1 <- rstan::extract(fit1)

  # fit normal distribution to second group (if second group is provided)
  extract2 <- NULL
  if (!is.null(y2)) {
    fit2 <- ttest_fit(y2)
    extract2 <- rstan::extract(fit2)
  }

  # cast rope interval to a list with 2 elements
  if (length(rope) == 1) {
    rope[2] <- rope[1]
    rope[1] <- -rope[1]
  }

  # order ascending
  rope <- sort(rope)

  # create output class
  out <- ttest_results(y1_samples = extract1, y2_samples = extract2, mu = mu, sigma = sigma, rope = rope)

  # return
  return(out)
}
