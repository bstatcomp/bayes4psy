# libs
library(bayes4psy)
library(ggplot2)
library(dplyr)

## load data ------------------------------------------------------------------
data <- read.table("../examples/data/adaptation_level.csv", sep="\t", header=TRUE)


## group 1 fitting ------------------------------------------------------------
group1 <- data[data$group == 1, ]

# number of subjects
m1 <- length(unique(group1$subject))

# map subject to 1..m interval
group1$subject <- plyr::mapvalues(group1$subject, from=unique(group1$subject), to=1:m1)

# split data to (part == 1 and part == 2)
group1_part1 <- group1 %>% filter(part == 1)
group1_part2 <- group1 %>% filter(part == 2)

# fit
fit1_part1 <- b_linear(x = group1_part1$sequence,
                       y = group1_part1$response,
                       s = group1_part1$subject)

fit1_part2 <- b_linear(x = group1_part2$sequence,
                       y = group1_part2$response,
                       s = group1_part2$subject)

## diagnose group 1 fits ------------------------------------------------------
# plot trace
plot_trace(fit1_part1)
plot_trace(fit1_part2)

# check fits
plot_fit(fit1_part1)
plot_fit(fit1_part1, subjects=TRUE)
plot_fit(fit1_part2)
plot_fit(fit1_part2, subjects=TRUE)

# check n_eff and RHat
print(fit1_part1)
print(fit1_part2)


## group 2 fitting ------------------------------------------------------------
group2 <- data[data$group == 2, ]

# number of subjects
m2 <- length(unique(group2$subject))

# map subject to 1..m interval
group2$subject <- plyr::mapvalues(group2$subject, from=unique(group2$subject), to=1:m2)

# split data to (part == 1 and part == 2)
group2_part1 <- group2 %>% filter(part == 1)
group2_part2 <- group2 %>% filter(part == 2)

# fit
fit2_part1 <- b_linear(x = group2_part1$sequence,
                       y = group2_part1$response,
                       s = group2_part1$subject)

fit2_part2 <- b_linear(x = group2_part2$sequence,
                       y = group2_part2$response,
                       s = group2_part2$subject)


## diagnose group 2 fits ------------------------------------------------------
# plot trace
plot_trace(fit2_part1)
plot_trace(fit2_part2)

# check fits
plot_fit(fit2_part1)
plot_fit(fit2_part1, subjects=TRUE)
plot_fit(fit2_part2)
plot_fit(fit2_part2, subjects=TRUE)

# check n_eff and RHat
print(fit2_part1)
print(fit2_part2)


## comparison and visualizations ----------------------------------------------
# compare_means
fit1_comparison <- compare_means(fit1_part1, fit2=fit1_part2)
fit2_comparison <- compare_means(fit2_part1, fit2=fit2_part2)


# difference
difference_group1 <- plot_means_difference(fit1_part1, fit2=fit1_part2, par="slope") +
  ggtitle("Group 1")
difference_group2 <- plot_means_difference(fit2_part1, fit2=fit2_part2, par="slope") +
  ggtitle("Group 2")

cowplot::plot_grid(difference_group1, difference_group2, ncol=2, nrow=1, scale=0.9)


# visually compare means
means_group1 <- plot_means(fit1_part1, fit2=fit1_part2, par="slope") +
  ggtitle("Group 1")
means_group2 <- plot_means(fit2_part1, fit2=fit2_part2, par="slope") +
  ggtitle("Group 2")

cowplot::plot_grid(means_group1, means_group2, ncol=2, nrow=1, scale=0.9)


# plot_distribution also add labels and same limits for both graphs
distributions_part1 <- plot_distributions(fit1_part1, fit2_part1) +
  xlim(0, 10) +
  ylim(0, 10) +
  labs(title="Part I", x="measurement number", y="weight") +
  theme(legend.position="none")

distributions_part2 <- plot_distributions(fit1_part2, fit2_part2) +
  xlim(0, 10) +
  ylim(0, 10) +
  labs(title="Part II", x="measurement number", y="") +
  theme(legend.position=) +
  theme(legend.position="none")

cowplot::plot_grid(distributions_part1, distributions_part2, ncol=2, nrow=1, scale=0.9)


## t-tests --------------------------------------------------------------------

## comparison of adaptation immediately after switching the weights -----------
group1_start <- group1_part2 %>% filter(sequence == 1 | sequence == 2)
group2_start <- group2_part2 %>% filter(sequence == 1 | sequence == 2)

# fit
ttest_group1_start <- b_ttest(data=group1_start$response)
ttest_group2_start <- b_ttest(data=group2_start$response)

## diagnose ttest fits --------------------------------------------------------
# plot trace
plot_trace(ttest_group1_start)
plot_trace(ttest_group2_start)

# check fits
plot_fit(ttest_group1_start)
plot_fit(ttest_group2_start)

# check n_eff and RHat
print(ttest_group1_start)
print(ttest_group2_start)

# compare use +/- 1 weight grade as rope interval
ttest_comparison_start <- compare_means(ttest_group1_start, fit2=ttest_group2_start, rope=1)

# plot difference use +/- 1 weight grade as rope interval
plot_means_difference(ttest_group1_start, fit2=ttest_group2_start, rope=1)

# plot means
plot_means(ttest_group1_start, fit2=ttest_group2_start)

# get values
summary(ttest_group1_start)
summary(ttest_group2_start)


## comparison of adaptation at the end of experiment --------------------------
group1_end <- group1_part2 %>% filter(sequence == 9 | sequence == 10)
group2_end <- group2_part2 %>% filter(sequence == 9 | sequence == 10)

# fit
ttest_group1_end <- b_ttest(data=group1_end$response)
ttest_group2_end <- b_ttest(data=group2_end$response)

## diagnose ttest fits --------------------------------------------------------
# plot trace
plot_trace(ttest_group1_end)
plot_trace(ttest_group2_end)

# check fits
plot_fit(ttest_group1_end)
plot_fit(ttest_group2_end)

.# check n_eff and RHat
print(ttest_group1_end)
print(ttest_group2_end)

# compare use +/- 1 weight grade as rope interval
ttest_comparison_end <- compare_means(ttest_group1_end, fit2=ttest_group2_end, rope=1)

# plot difference use +/- 1 weight grade as rope interval
plot_means_difference(ttest_group1_end, fit2=ttest_group2_end, rope=1)

# plot means
plot_means(ttest_group1_end, fit2=ttest_group2_end)

# get values
summary(ttest_group1_end)
summary(ttest_group2_end)
