# libs
library(EasyBayes)

# number of measurements
n <- 1000

# height of females from brazil
y_brazil <- rnorm(n, 155.7, 6.6)

# a distribution
mu <- 165
sigma = 10

# ttest, rope interval 2 cm
ttest_results <- ttest_bayes(y1 = y_brazil, mu = mu, sigma = sigma, rope = 2)

# summary is the same as just witing down the object - one could just write ttest_results
summary(ttest_results)

# difference plot
difference_plot(ttest_results)

# comparison plot
comparison_plot(ttest_results)
