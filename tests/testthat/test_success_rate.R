library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# priors
p_prior <- b_prior(family="beta", pars=c(1, 1))
tau_prior <- b_prior(family="uniform", pars=c(0, 500))

# attach priors to relevant parameters
priors <- list(c("p", p_prior),
               c("tau", tau_prior))

# subjects
s <- rep(1:5, 20)

# generate data and fit
data1 <- rbinom(100, size=1, prob=0.6)
fit1 <- b_success_rate(r=data1, s=s, priors=priors, chains=1, refresh=0)

data2 <- rbinom(100, size=1, prob=0.1)
fit2 <- b_success_rate(r=data2, s=s, priors=priors, chains=1, refresh=0)

data3 <- rbinom(100, size=1, prob=0.5)
fit3 <- b_success_rate(r=data3, s=s, priors=priors, chains=1, refresh=0)

# fit list
fit_list <- list(fit2, fit3)


# summary
test_that("success_rate summary", {
  output <- capture.output(summary(fit1))
  compare <- "Success rate:\t0.57 +/- 0.00500\t95% HDI: [0.48, 0.66]"
  expect_equal(compare, output)
})


# print and show
test_that("success_rate print and show", {
  output <- capture.output(print(fit1))
  output <- output[6]
  compare <- "p0     0.57    0.01   0.05   0.48   0.54   0.57   0.61   0.66    68 1.02"
  expect_equal(compare, output)
})


# get_parameters
test_that("success_rate get_parameters", {
  parameters <- get_parameters(fit1)
  expect_equal(parameters$p[1], 0.5446301, tolerance=1e-4)
  expect_equal(parameters$tau[1], 473.2385, tolerance=1e-4)
})


# get_subject_parameters
test_that("success_rate get_subject_parameters", {
  parameters <- get_subject_parameters(fit1)
  expect_equal(parameters$p[1], 0.5482244, tolerance=1e-4)
})


# compare_means two fits
test_that("success_rate compare_means two fits", {
  o <- capture.output(output <- compare_means(fit1, fit2=fit2))
  expect_equal(output[1, 2], 1)
  expect_equal(output[2, 1], 0)
})


# compare_means multiple fits
test_that("success_rate compare_means multiple fits", {
  o <- capture.output(output <- compare_means(fit1, fits=fit_list))
  expect_equal(output$comparison_matrix[1,], c(NA, 1.00, 0.87))
  expect_equal(output$smallest_largest$smallest, c(0, 1, 0))
})


# compare_distributions two fits
test_that("success_rate compare_distributions two fits", {
  o <- capture.output(output <- compare_distributions(fit1, fit2=fit2))
  expect_equal(output[1, 2], 1)
  expect_equal(output[2, 1], 0)
})


# compare_distributions multiple fits
test_that("success_rate compare_distributions multiple fits", {
  o <- capture.output(output <- compare_distributions(fit1, fits=fit_list))
  expect_equal(output$comparison_matrix[1,], c(NA, 1, 0.96))
  expect_equal(output$smallest_largest$smallest, c(0, 1, 0))
})
