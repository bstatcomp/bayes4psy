context("test-ttest_stan.R")

test_that("ttest works", {
  set.seed(0)

  # source https://doi.org/10.1371/journal.pone.0018962
  # height of females from brazil
  y_brazil = rnorm(1000, 155.7, 6.6)

  # height of females from cameroon
  y_cameroon = rnorm(1000, 160.4, 6.3)

  # test
  ttest_results <- ttest_bayes(y_brazil, y_cameroon, ROPE = 1)

  expect_equal(mean(ttest_results@y1_samples$mu), 155.7, tolerance = 1)
  expect_equal(mean(ttest_results@y1_samples$sigma), 6.6, tolerance = 0.2)
  expect_equal(mean(ttest_results@y2_samples$mu), 160.4, tolerance = 1)
  expect_equal(mean(ttest_results@y2_samples$sigma), 6.3, tolerance = 0.2)
})
