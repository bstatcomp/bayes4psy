context("test-normal_stan.R")

test_that("normal fit works", {
  set.seed(0)

  y <- rnorm(100, 5, 1)

  expect_warning(normal_fit <- normal_stan(y, seed = 0))

  normal_extract <- rstan::extract(normal_fit)

  expect_equal(mean(normal_extract$mu), 4.98045, tolerance = 1e-5)

  expect_equal(mean(normal_extract$sigma), 0.89265, tolerance = 1e-5)
})
