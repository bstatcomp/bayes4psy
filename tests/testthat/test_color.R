library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# priors for rgb
mu_prior <- b_prior(family="uniform", pars=c(0, 255))
sigma_prior <- b_prior(family="uniform", pars=c(0, 100))

# attach priors to relevant parameters
priors_rgb <- list(c("mu_r", mu_prior),
                   c("sigma_r", sigma_prior),
                   c("mu_g", mu_prior),
                   c("sigma_g", sigma_prior),
                   c("mu_b", mu_prior),
                   c("sigma_b", sigma_prior))


# generate data (rgb) and fit
r <- as.integer(rnorm(100, mean=250, sd=20))
r[r > 255] <- 255
r[r < 0] <- 0

g <- as.integer(rnorm(100, mean=20, sd=20))
g[g > 255] <- 255
g[g < 0] <- 0

b <- as.integer(rnorm(100, mean=40, sd=20))
b[b > 255] <- 255
b[b < 0] <- 0

colors <- data.frame(r=r, g=g, b=b)

fit1 <- b_color(colors=colors, priors=priors_rgb, chains=1, refresh=0)


# priors for hsv
h_prior <- b_prior(family="uniform", pars=c(0, 2*pi))
sv_prior <- b_prior(family="uniform", pars=c(0, 1))
kappa_prior <- b_prior(family="uniform", pars=c(0, 500))
sigma_prior <- b_prior(family="uniform", pars=c(0, 1))

# attach priors to relevant parameters
priors_hsv <- list(c("mu_h", h_prior),
                   c("kappa_h", kappa_prior),
                   c("mu_s", sv_prior),
                   c("sigma_s", sigma_prior),
                   c("mu_v", sv_prior),
                   c("sigma_v", sigma_prior))

# generate data (hsv) and fit
h <- rnorm(100, mean=2*pi/3, sd=0.5)
h[h > 2*pi] <- 2*pi
h[h < 0] <- 0

s <- rnorm(100, mean=0.9, sd=0.2)
s[s > 1] <- 1
s[s < 0] <- 0

v <- rnorm(100, mean=0.9, sd=0.2)
v[v > 1] <- 1
v[v < 0] <- 0

colors <- data.frame(h=h, s=s, v=v)

fit2 <- b_color(colors=colors, hsv=TRUE, priors=priors_hsv, chains=1, refresh=0)


# summary
test_that("color summary", {
  output <- capture.output(summary(fit1))
  compare_mu_h <- "mu_h:\t\t-0.10 +/- 0.00026\t95% HDI: [-0.12, -0.07]"
  compare_kappa_h <- "kappa_h:\t68.33 +/- 0.24086\t95% HDI: [48.79, 86.95]"
  expect_equal(compare_mu_h, output[7])
  expect_equal(compare_kappa_h, output[8])
})


# print and show
test_that("color print and show", {
  output <- capture.output(print(fit1))
  output <- output[12]
  compare <- "mu_h      -0.10    0.00 0.01   -0.12   -0.11   -0.10   -0.09   -0.08  2457    1"
  expect_equal(compare, output)
})


# get_parameters
test_that("color get_parameters", {
  parameters <- get_parameters(fit1)
  expect_equal(parameters$h[1], -0.106492, tolerance=1e-4)
  expect_equal(parameters$s[1], 0.937346, tolerance=1e-4)
  expect_equal(parameters$v[1], 0.957944, tolerance=1e-4)
})


# compare_means two fits
test_that("color compare_means two fits", {
  o <- capture.output(output <- compare_means(fit1, fit2=fit2))
  r <- c(0, 1, NA)
  g <- c(1, 0, NA)
  b <- c(1, 0, NA)
  h <- c(1, 0, NA)
  s <- c(0, 1, NA)
  v <- c(0, 1, NA)
  compare <- rbind(r, g, b, h, s, v)
  expect_equal(output, compare)
})


# compare_distributions two fits
test_that("color compare_distributions two fits", {
  o <- capture.output(output <- compare_distributions(fit1, fit2=fit2))
  r <- c(0.01, 0.99, NA)
  g <- c(1.00, 0.00, NA)
  b <- c(0.63, 0.37, NA)
  h <- c(1.00, 0.00, NA)
  s <- c(0.32, 0.68, NA)
  v <- c(0.27, 0.73, NA)
  compare <- rbind(r, g, b, h, s, v)
  expect_equal(output, compare)
})
