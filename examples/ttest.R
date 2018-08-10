# libs
library(EasyBayes)

# number of measurements
n <- 1000

# height of females from brazil
y_brazil = rnorm(1000, 155.7, 6.6)

# height of females from cameroon
y_cameroon = rnorm(1000, 160.4, 6.3)

# ttest, rope interval 1 cm
ttest_results <- ttest_bayes(y_brazil, y_cameroon, ROPE = 1)

# difference
ttest_results

# summary is the same as just wiring down the object
summary(ttest_results)

# difference plot
difference_plot(ttest_results)
