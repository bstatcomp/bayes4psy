library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# set tolerance
tol <- 0.5

# priors
mu_prior <- b_prior(family="normal", pars=c(0, 100))
sigma_prior <- b_prior(family="uniform", pars=c(0, 500))

# attach priors to relevant parameters
priors <- list(c("mu_a", mu_prior),
               c("sigma_a", sigma_prior),
               c("mu_b", mu_prior),
               c("sigma_b", sigma_prior),
               c("mu_s", sigma_prior),
               c("sigma_s", sigma_prior))


# generate data and fit
x <- vector()
y <- vector()
s <- vector()
for (i in 1:5) {
  x <- c(x, rep(1:10))
  y <- c(y, rnorm(10, mean=1:10, sd=2))
  s <- c(s, rep(i, 10))
}

fit1 <- b_linear(x=x, y=y, s=s, priors=priors, chains=1, seed=seed, refresh=0)

fit2 <- b_linear(x=x, y=-2*y, s=s, priors=priors, chains=1, seed=seed, refresh=0)


# summary
test_that("linear summary", {
 expect_output(summary(fit1), regexp="intercept")
 expect_output(summary(fit1), regexp="slope")
 expect_output(summary(fit1), regexp="sigma")
})


# print and show
test_that("linear print and show", {
  expect_output(print(fit1),  regexp="mu_a")
  expect_output(print(fit1),  regexp="mu_b")
  expect_output(print(fit1),  regexp="mu_s")
})


# get_parameters
test_that("linear get_parameters", {
  parameters <- get_parameters(fit1)
  expect_equal(mean(parameters$slope), 0.2, tolerance=tol)
  expect_equal(mean(parameters$intercept), 0.97, tolerance=tol)
  expect_equal(mean(parameters$sigma), 1.94, tolerance=tol)
})


# get_subject_parameters
test_that("linear get_subject_parameters", {
  parameters <- get_subject_parameters(fit1)
  expect_equal(mean(parameters$slope), 0.19, tolerance=tol)
  expect_equal(mean(parameters$intercept), 0.97, tolerance=tol)
  expect_equal(mean(parameters$sigma), 1.92, tolerance=tol)
})


# compare_means two fits
test_that("linear compare_means two fits", {
  o <- capture.output(output <- compare_means(fit1, fit2=fit2))
  intercept <- c(0.32, 0.68, NA)
  slope <- c(0, 1, NA)
  compare <- rbind(intercept, slope)
  expect_equal(output, compare, tolerance=tol)
})


# compare_distributions two fits
test_that("linear compare_distributions two fits", {
  o <- capture.output(output <- compare_distributions(fit1, fit2=fit2))
  intercept <- c(0.32, 0.68, NA)
  slope <- c(0.0, 1.0, NA)
  compare <- rbind(intercept, slope)
  expect_equal(output, compare, tolerance=tol)
})
