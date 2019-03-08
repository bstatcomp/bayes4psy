# author: John Kruschke
mcmc_hdi <- function(samples, cred_mass=0.95) {
  samples <- sort(samples)
  ci_ceil <- ceiling(cred_mass * length(samples))
  n <- length(samples) - ci_ceil

  ci_width <- rep(0, n)
  for (i in 1:n) {
    ci_width[i] <- samples[i + ci_ceil] - samples[i]
  }

  hdi_min <- samples[which.min(ci_width)]
  hdi_max <- samples[which.min(ci_width) + ci_ceil]
  hdi <- c(hdi_min, hdi_max)
  return(hdi)
}
