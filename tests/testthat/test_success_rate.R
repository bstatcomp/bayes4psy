library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# set tolerance
tol <- 0.2

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
fit1 <- b_success_rate(r=data1, s=s, priors=priors, chains=1, seed=seed, refresh=0)

data2 <- rbinom(100, size=1, prob=0.1)
fit2 <- b_success_rate(r=data2, s=s, priors=priors, chains=1, seed=seed, refresh=0)

data3 <- rbinom(100, size=1, prob=0.5)
fit3 <- b_success_rate(r=data3, s=s, priors=priors, chains=1, seed=seed, refresh=0)

# fit list
fit_list <- list(fit2, fit3)


# summary
test_that("success_rate summary", {
 expect_output(summary(fit1), regexp="Success rate")
})


# print and show
test_that("success_rate print and show", {
 expect_output(print(fit1), regexp="p0")
 expect_output(print(fit1), regexp="tau")
})


# get_parameters
test_that("success_rate get_parameters", {
 parameters <- get_parameters(fit1)
 expect_equal(mean(parameters$p), 0.57, tolerance=tol)
})


# get_subject_parameters
test_that("success_rate get_subject_parameters", {
 parameters <- get_subject_parameters(fit1)
 expect_equal(mean(parameters$p), 0.57, tolerance=tol)
})


# compare_means two fits
test_that("success_rate compare_means two fits", {
 o <- capture.output(output <- compare_means(fit1, fit2=fit2))
 expect_equal(output[1, 2], 1, tolerance=tol)
 expect_equal(output[2, 1], 0, tolerance=tol)
})


# compare_means multiple fits
test_that("success_rate compare_means multiple fits", {
 o <- capture.output(output <- compare_means(fit1, fits=fit_list))
 expect_equal(output$comparison_matrix[1,], c(NA, 1.00, 0.96), tolerance=tol)
 expect_equal(output$smallest_largest$smallest, c(0, 1, 0), tolerance=tol)
})


# compare_distributions two fits
test_that("success_rate compare_distributions two fits", {
 o <- capture.output(output <- compare_distributions(fit1, fit2=fit2))
 expect_equal(output[1, 2], 1, tolerance=tol)
 expect_equal(output[2, 1], 0, tolerance=tol)
})


# compare_distributions multiple fits
test_that("success_rate compare_distributions multiple fits", {
 o <- capture.output(output <- compare_distributions(fit1, fits=fit_list))
 expect_equal(output$comparison_matrix[1,], c(NA, 1, 1), tolerance=tol)
 expect_equal(output$smallest_largest$smallest, c(0, 1, 0), tolerance=tol)
})
