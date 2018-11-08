#' Bayesian t-test.
#'
#' @export
#' @param y1 Numeric vector of values for the first group.
#' @param y2 Optional parameter for values for the second group.
#' @param mu Optional parameter for comparison of first group against a mean value.
#' @param sigma Optional parameter for comparison of first group against a distribution N(mu, sigma).
#' @param rope Optional parameter for region of practical equivalence, default = NULL
#' @return An object of class `ttest_results_class`.
#'
b_ttest <- function(y1,
                    y2 = NULL,
                    mu = NULL,
                    sigma = NULL,
                    rope = NULL) {

  # prepare data
  n1 <- length(y1)
  n2 <- 0
  # number of groups
  g <- 1

  # two groups provided?
  if (!is.null(y2)) {
    g <- 2
    n2 <- length(y2)
  }

  # all data
  y <- c(y1, y2)

  # group ids
  g_id <- c(rep(1, n1), rep(2, n2))

  # used for priors
  yMu <- mean(y)
  ySd <- sd(y)

  stan_data <- list(
    g = g,
    n = length(y),
    g_id = g_id,
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
  out <- ttest_results(extract = extract, fit = fit, mu = mu, sigma = sigma, rope = rope, data1 = y1, data2 = y2)

  # return
  return(out)
}
