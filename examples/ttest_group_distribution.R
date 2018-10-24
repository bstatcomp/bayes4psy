# libs
library(EasyBayes)

# number of measurements
n <- 25

# height of females from brazil
y_brazil <- rnorm(n, 155.7, 6.6)

# a distribution
mu <- 165
sigma = 10

# ttest, rope interval 1 cm
ttest_results <- ttest_bayes(y1 = y_brazil, mu = mu, sigma = sigma, rope = 1)

# you can later change value of the rope interval
ttest_results@rope <- 2

# print summary
ttest_results

# summary is the same as just witing down the object - one could just write ttest_results
summary(ttest_results)

# difference plot
plot_difference(ttest_results)

# comparison plot
plot_comparison(ttest_results)

# compare distributions
compare_distributions(ttest_results)

# distribution difference plot
plot_distributions_difference(ttest_results)

# distributions plot
plot_distributions(ttest_results)
