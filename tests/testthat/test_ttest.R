library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# priors
mu_prior <- b_prior(family="normal", pars=c(0, 1000))
sigma_prior <- b_prior(family="uniform", pars=c(0, 500))

# attach priors to relevant parameters
priors <- list(c("mu", mu_prior),
               c("sigma", sigma_prior))

# generate data and fit
data1 <- rnorm(20, mean=150, sd=20)
fit1 <- b_ttest(data=data1, priors=priors, chains=1, seed=seed, refresh=0)

data2 <- rnorm(20, mean=200, sd=20)
fit2 <- b_ttest(data=data2, priors=priors, chains=1, seed=seed, refresh=0)

data3 <- rnorm(20, mean=150, sd=40)
fit3 <- b_ttest(data=data3, priors=priors, chains=1, seed=seed, refresh=0)

# fit list
fit_list <- list(fit2, fit3)


# summary
test_that("ttest summary", {
  output <- capture.output(summary(fit1))
  compare <- NULL
  compare[1] <- "mu:\t\t149.83 +/- 0.16540\t95% HDI: [140.98, 159.43]"
  compare[2] <- "sigma:\t\t20.83 +/- 0.14330\t95% HDI: [13.04, 28.81]"
  compare[3] <- "nu:\t\t33.65 +/- 1.23163\t95% HDI: [1.41, 95.75]"
  expect_equal(compare, output)
})


# print and show
test_that("ttest print and show", {
  output <- capture.output(print(fit1))
  output <- output[7]
  compare <- "mu         149.83    0.17  4.86 140.76 146.44 149.81 153.00 159.22   848    1"
  expect_equal(compare, output)
})


# get_parameters
test_that("ttest get_parameters", {
  parameters <- get_parameters(fit1)
  expect_equal(parameters$mu[1], 143.0587, tolerance=1e-4)
  expect_equal(parameters$sigma[1], 20.58253, tolerance=1e-4)
  expect_equal(parameters$nu[1], 46.62235, tolerance=1e-4)
})


# compare_means two fits
test_that("ttest compare_means two fits", {
  o <- capture.output(output <- compare_means(fit1, fit2=fit2))
  expect_equal(output[1, 2], 0)
  expect_equal(output[2, 1], 1)
})


# compare_means fit and mu
test_that("ttest compare_means fit and mu", {
  o <- capture.output(output <- compare_means(fit1, mu=150))
  expect_equal(output[1, 2], 0.48)
  expect_equal(output[2, 1], 0.52)
})


# compare_means multiple fits
test_that("ttest compare_means multiple fits", {
  o <- capture.output(output <- compare_means(fit1, fits=fit_list))
  expect_equal(output$comparison_matrix[1,], c(NA, 0.0, 0.4))
  expect_equal(output$smallest_largest$smallest, c(0.604, 0.000, 0.396))
})


# compare_distributions two fits
test_that("ttest compare_distributions two fits", {
  o <- capture.output(output <- compare_distributions(fit1, fit2=fit2))
  expect_equal(output[1, 2], 0.03)
  expect_equal(output[2, 1], 0.97)
})


# compare_distributions multiple fits
test_that("ttest compare_distributions multiple fits", {
  o <- capture.output(output <- compare_distributions(fit1, fits=fit_list))
  expect_equal(output$comparison_matrix[1,], c(NA, 0.03, 0.48))
  expect_equal(output$smallest_largest$smallest, c(0.5145, 0.0105, 0.4750), tolerance=1e-2)
})
