library(bayes4psy)

# set seed
seed <- 0
set.seed(0)

# set tolerance
tol <- 0.2

# linear function of seqence vs. response
lm_statistic <- function(data) {
  lm(sequence ~ response, data)$coef
}

# load data
data <- adaptation_level_small

# bootstrap
data_bootstrap <- b_bootstrap(data, lm_statistic, n1=1000, n2=1000)

# get_parameters
test_that("bootstrap", {
  expect_equal(mean(data_bootstrap$response), -0.479, tolerance=tol)
  expect_equal(mean(data_bootstrap$`(Intercept)`), 8.754, tolerance=tol)
})
