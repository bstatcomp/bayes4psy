# libs
library(EasyBayes)


## generate data ---------------------------------------------------------
# number of measurements
n <- 25

# height of females from brazil
y_brazil <- rnorm(n, 155.7, 6.6)

# height of females from cameroon
y_cameroon <- rnorm(n, 160.4, 6.3)

# mu
mu <- 150

# sigma
sigma <- 10


## ttest fit -------------------------------------------------------------
ttest_results <- b_ttest(y = y_brazil)
ttest_results2 <- b_ttest(y = y_cameroon)

# print summary
summary(ttest_results)

# visualize fit quality
plot_fit(ttest_results)

# traceplot
traceplot(ttest_results)

# samples for single fit
plot_samples(ttest_results)


## compare ---------------------------------------------------------------
# compare both fitted objects
compare(ttest_results, fit2 = ttest_results2)

# you can also provide rope interval
compare(ttest_results, fit2 = ttest_results2, rope = 2)

# compare fitted object with a constant value
compare(ttest_results, mu = mu)

# compare fitted object with a normal distribution (used here in Cohen's d calculation)
compare(ttest_results, mu = mu, sigma = sigma)


## plot difference -------------------------------------------------------
# difference between two fitted objects
plot_difference(ttest_results, fit2 = ttest_results2)

# you can also provide the rope and bins parameters
plot_difference(ttest_results, fit2 = ttest_results2, rope = 2, bins = 10)

# difference between a fitted object and a mean value
plot_difference(ttest_results, mu = mu)


## plot samples ----------------------------------------------------------
# comparison between two fitted objects
plot_samples(ttest_results, fit2 = ttest_results2)

# comparison between a fit and a mean value (or the mean of a normal distribution)
plot_samples(ttest_results, mu = mu)


## compare distributions -------------------------------------------------------
# compare distributions of two fitted objects
compare_distributions(ttest_results, fit2 = ttest_results2)

# you can also provide the rope interval
compare_distributions(ttest_results, fit2 = ttest_results2, rope = 2)

# compare fitted distribution with a constant value
compare_distributions(ttest_results, mu = mu)

# compare fitted distribution with a normal distribution (used here in Cohen's d calculation)
compare_distributions(ttest_results, mu = mu, sigma = sigma)


## plot disributions -------------------------------------------------------
# plot distributions of two fitted objects
plot_distributions(ttest_results, fit2 = ttest_results2)

# plot distribution of a fitted object and a constant value
plot_distributions(ttest_results, mu = mu)

# plot distribution of a fitted object and a normal distribution
plot_distributions(ttest_results, mu = mu, sigma = sigma)

## plot disributions difference ------------------------------------------
# plot distribution difference between two fitted objects
plot_distributions_difference(ttest_results, fit2 = ttest_results2)

# you can also provide the rope and bins parameters
plot_distributions_difference(ttest_results, fit2 = ttest_results2, rope = 5, bins = 10)

# plot distribution difference between a fitted object and a constant value
plot_distributions_difference(ttest_results, mu = mu)

# plot distribution difference between a fitted object and a normal distribution
plot_distributions_difference(ttest_results, mu = mu, sigma = sigma)
