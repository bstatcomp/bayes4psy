library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# set tolerance
tol <- 0.2

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
 expect_output(summary(fit1), regexp="mu")
 expect_output(summary(fit1), regexp="sigma")
 expect_output(summary(fit1), regexp="nu")
})


# print and show
test_that("ttest print and show", {
  expect_output(summary(fit1), regexp="mu")
  expect_output(summary(fit1), regexp="sigma")
  expect_output(summary(fit1), regexp="nu")
})


# get_parameters
test_that("ttest get_parameters", {
 parameters <- get_parameters(fit1)
 expect_equal(mean(parameters$mu), 149.58, tolerance=tol)
 expect_equal(mean(parameters$sigma), 21.16, tolerance=tol)
 expect_equal(mean(parameters$nu), 33.58, tolerance=tol)
})


# compare_means two fits
test_that("ttest compare_means two fits", {
 o <- capture.output(output <- compare_means(fit1, fit2=fit2))
 expect_equal(output[1, 2], 0, tolerance=tol)
 expect_equal(output[2, 1], 1, tolerance=tol)
})


# compare_means fit and mu
test_that("ttest compare_means fit and mu", {
 o <- capture.output(output <- compare_means(fit1, mu=150))
 expect_equal(output[1, 2], 0.47, tolerance=tol)
 expect_equal(output[2, 1], 0.53, tolerance=tol)
})


# compare_means multiple fits
test_that("ttest compare_means multiple fits", {
 o <- capture.output(output <- compare_means(fit1, fits=fit_list))
 expect_equal(output$comparison_matrix[1,], c(NA, 0.0, 0.39), tolerance=tol)
 expect_equal(output$smallest_largest$smallest, c(0.6, 0, 0.4), tolerance=tol)
})


# compare_distributions two fits
test_that("ttest compare_distributions two fits", {
 o <- capture.output(output <- compare_distributions(fit1, fit2=fit2))
 expect_equal(output[1, 2], 0.03, tolerance=tol)
 expect_equal(output[2, 1], 0.97, tolerance=tol)
})


# compare_distributions multiple fits
test_that("ttest compare_distributions multiple fits", {
 o <- capture.output(output <- compare_distributions(fit1, fits=fit_list))
 expect_equal(output$comparison_matrix[1,], c(NA, 0.03, 0.47), tolerance=tol)
 expect_equal(output$smallest_largest$smallest, c(0.52, 0.01, 0.47), tolerance=tol)
})
