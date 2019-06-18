library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# priors
mu_prior <- b_prior(family="normal", pars=c(0, 100))
sigma_prior <- b_prior(family="uniform", pars=c(0, 500))
lambda_prior <- b_prior(family="uniform", pars=c(0.05, 5))

# attach priors to relevant parameters
priors <- list(c("mu_m", mu_prior),
               c("sigma_m", sigma_prior),
               c("mu_s", sigma_prior),
               c("sigma_s", sigma_prior),
               c("mu_l", lambda_prior),
               c("sigma_l", sigma_prior))


# subjects
s <- rep(1:5, 20)

# generate data and fit
rt1 <- emg::remg(100, mu=10, sigma=1, lambda=0.4)
fit1 <- b_reaction_time(t=rt1, s=s, priors=priors, chains=1, refresh=0)

rt2 <- emg::remg(100, mu=10, sigma=2, lambda=0.1)
fit2 <- b_reaction_time(t=rt2, s=s, priors=priors, chains=1, refresh=0)

rt3 <- emg::remg(100, mu=20, sigma=2, lambda=1)
fit3 <- b_reaction_time(t=rt3, s=s, priors=priors, chains=1, refresh=0)

# fit list
fit_list <- list(fit2, fit3)


# summary
test_that("reaction_time summary", {
  output <- capture.output(summary(fit1))
  compare <- NULL
  compare[1] <- "rt:\t\t12.39 +/- 0.04509\t95% HDI: [11.79, 13.22]"
  compare[2] <- "mu:\t\t11.87 +/- 0.09029\t95% HDI: [10.60, 13.06]"
  compare[3] <- "sigma:\t\t1.98 +/- 0.04645\t95% HDI: [1.10, 2.62]"
  compare[4] <- "lambda:\t\t3.0112 +/- 0.21227\t95% HDI: [0.5911, 4.9687]"
  expect_equal(compare, output)
})


# print and show
test_that("reaction_time print and show", {
  output <- capture.output(print(fit1))
  output <- output[21]
  compare <- "mu_m             11.87    0.13 0.61   10.28   11.67   11.91   12.17   12.96"
  expect_equal(compare, output)
})


# get_parameters
test_that("reaction_time get_parameters", {
  parameters <- get_parameters(fit1)
  expect_equal(parameters$rt[1], 12.18368, tolerance=1e-4)
  expect_equal(parameters$mu[1], 11.76037, tolerance=1e-4)
  expect_equal(parameters$sigma[1], 1.863798, tolerance=1e-4)
  expect_equal(parameters$lambda[1], 2.362304, tolerance=1e-4)
})


# get_subject_parameters
test_that("reaction_time get_subject_parameters", {
  parameters <- get_subject_parameters(fit1)
  expect_equal(parameters$rt[1], 12.51732, tolerance=1e-4)
  expect_equal(parameters$mu[1], 12.2047, tolerance=1e-4)
  expect_equal(parameters$sigma[1], 1.800518, tolerance=1e-4)
  expect_equal(parameters$lambda[1], 3.198721, tolerance=1e-4)
})


# compare_means two fits
test_that("reaction_time compare_means two fits", {
  o <- capture.output(output <- compare_means(fit1, fit2=fit2))
  expect_equal(output[1, 2], 0)
  expect_equal(output[2, 1], 1)
})


# compare_means multiple fits
test_that("reaction_time compare_means multiple fits", {
  o <- capture.output(output <- compare_means(fit1, fits=fit_list))
  expect_equal(output$comparison_matrix[1,], c(NA, 0, 0))
  expect_equal(output$smallest_largest$smallest, c(1, 0, 0))
})


# compare_distributions two fits
test_that("reaction_time compare_distributions two fits", {
  o <- capture.output(output <- compare_distributions(fit1, fit2=fit2))
  expect_equal(output[1, 2], 0.17)
  expect_equal(output[2, 1], 0.83)
})


# compare_distributions multiple fits
test_that("reaction_time compare_distributions multiple fits", {
  o <- capture.output(output <- compare_distributions(fit1, fits=fit_list))
  expect_equal(output$comparison_matrix[1,], c(NA, 0.17, 0.00))
  expect_equal(output$smallest_largest$smallest, c(0.82861, 0.16929, 0.00210), tolerance=1e-4)
})
