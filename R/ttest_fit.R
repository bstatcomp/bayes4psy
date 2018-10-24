#' Bayesian Student's t-test model fitting.
#'
#' @param y1 Numeric vector of values for the first group.
#' @param y2 Optional parameter for values for the second group.
#' @return An object of class `stanfit` returned by `rstan::sampling`.
#'
ttest_fit <- function(y1, y2 = NULL) {
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

  out <- rstan::sampling(stanmodels$ttest,
    data = stan_data,
    chains = 1,
    warmup = 12000,
    iter = 15000
  )

  return(out)
}
