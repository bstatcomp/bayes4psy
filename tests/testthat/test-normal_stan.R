context("test-normal_stan.R")

test_that("normal fit works", {
  set.seed(0)

  # generate data - N(5, 1)
  y <- rnorm(500, 5, 1)

  # fit
  expect_warning(normal_fit <- normal_stan(y))

  # extract
  normal_extract <- rstan::extract(normal_fit)

  # compare
  expect_equal(mean(normal_extract$mu), 5, tolerance = 1e-2)
  expect_equal(mean(normal_extract$sigma), 1, tolerance = 1e-2)
})
