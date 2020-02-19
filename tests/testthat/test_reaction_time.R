library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# set tolerance
tol <- 0.2

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
s <- rep(1:5, 10)

# generate data and fit
rt1 <- emg::remg(50, mu=10, sigma=1, lambda=0.4)
fit1 <- b_reaction_time(t=rt1, s=s, priors=priors, chains=1, seed=seed, refresh=0)

rt2 <- emg::remg(50, mu=20, sigma=2, lambda=0.1)
fit2 <- b_reaction_time(t=rt2, s=s, priors=priors, chains=1, seed=seed, refresh=0)

rt3 <- emg::remg(50, mu=10, sigma=2, lambda=1)
fit3 <- b_reaction_time(t=rt3, s=s, priors=priors, chains=1, seed=seed, refresh=0)

# fit list
fit_list <- list(fit2, fit3)


# summary
test_that("reaction_time summary", {
 expect_output(summary(fit1), regexp="rt")
 expect_output(summary(fit1), regexp="mu")
 expect_output(summary(fit1), regexp="sigma")
 expect_output(summary(fit1), regexp="lambda")
})


# print and show
test_that("reaction_time print and show", {
 expect_output(print(fit1), regexp="mu_m")
 expect_output(print(fit1), regexp="mu_l")
 expect_output(print(fit1), regexp="mu_s")
})


# get_parameters
test_that("reaction_time get_parameters", {
 parameters <- get_parameters(fit1)
 expect_equal(mean(parameters$rt), 11.90, tolerance=tol)
})


# get_subject_parameters
test_that("reaction_time get_subject_parameters", {
 parameters <- get_subject_parameters(fit1)
 expect_equal(mean(parameters$rt), 12.00, tolerance=tol)
})


# compare_means two fits
test_that("reaction_time compare_means two fits", {
 o <- capture.output(output <- compare_means(fit1, fit2=fit2))
 expect_equal(output[1, 2], 0, tolerance=tol)
 expect_equal(output[2, 1], 1, tolerance=tol)
})


# compare_means multiple fits
test_that("reaction_time compare_means multiple fits", {
 o <- capture.output(output <- compare_means(fit1, fits=fit_list))
 expect_equal(output$comparison_matrix[1,], c(NA, 0, 0.94), tolerance=tol)
 expect_equal(output$smallest_largest$smallest, c(0.06, 0, 0.94), tolerance=tol)
})


# compare_distributions two fits
test_that("reaction_time compare_distributions two fits", {
 o <- capture.output(output <- compare_distributions(fit1, fit2=fit2))
 expect_equal(output[1, 2], 0.03, tolerance=tol)
 expect_equal(output[2, 1], 0.97, tolerance=tol)
})


# compare_distributions multiple fits
test_that("reaction_time compare_distributions multiple fits", {
 o <- capture.output(output <- compare_distributions(fit1, fits=fit_list))
 expect_equal(output$comparison_matrix[1,], c(NA, 0.03, 0.66), tolerance=tol)
 expect_equal(output$smallest_largest$smallest, c(0.33, 0.01, 0.65), tolerance=tol)
})
