# libs
library(EasyBayes)

# number of measurements
n <- 25

# height of females from brazil
y_brazil <- rnorm(n, 155.7, 6.6)

# height of females from cameroon
y_cameroon <- rnorm(n, 160.4, 6.3)

# ttest, rope interval 1 cm
ttest_results <- b_ttest(y1 = y_brazil, y2 = y_cameroon, rope = 1)

# you can later change value of the rope interval
ttest_results@rope <- 2

# print summary
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
