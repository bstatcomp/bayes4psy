# libs
library(EasyBayes)

# number of measurements
n <- 1000

# height of females from brazil
y_brazil <- rnorm(n, 155.7, 6.6)

# height of females from cameroon
y_cameroon <- rnorm(n, 160.4, 6.3)

# ttest, rope interval 2 cm
ttest_results <- ttest_bayes(y1 = y_brazil, y2 = y_cameroon, rope = 2)

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

# distributions plot
plot_distributions(ttest_results)

# distribution difference plot
plot_distributions_difference(ttest_results)
