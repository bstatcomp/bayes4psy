# libs
library(EasyBayes)

# number of measurements
n <- 1000

# height of females from brazil
y_brazil = rnorm(n, 155.7, 6.6)

# height of females from cameroon
y_cameroon = rnorm(n, 160.4, 6.3)

# ttest, rope interval 1 cm
ttest_results <- ttest_bayes(y_brazil, y_cameroon, ROPE = 5)

# summary is the same as just witing down the object - one could just write ttest_results
summary(ttest_results)

# difference plot
difference_plot(ttest_results)
