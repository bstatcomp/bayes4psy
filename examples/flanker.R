# libs
library(EasyBayes)


## data wrangling --------------------------------------------------------
# load data
df <- read.table("./examples/data/flanker.csv", sep = "\t", header = TRUE)

# map correct/incorrect/timeout to 1/0
df$result_numeric <- 0
df[df$result == "correct", ]$result_numeric <- 1


### REACTION TIMES - test vs control group -------------------------------
# test vs control (correct and no timeout only)
df_correct <- df[df$result == "correct", ]


## control group fit -----------------------------------------------------
df_control <- df_correct[df_correct$group == "control", ]

# subject indexes range on 22..45 cast to 1..23
df_control$subject <- df_control$subject - 21

n <- nrow(df_control)
m <- length(unique(df_control$subject))
rt <- df_control$rt
s <- df_control$subject

rt_control <- b_reaction_time(n = n, m = m, rt = rt, s = s)

# summary
summary(rt_control)

# check fits
plot_fit(rt_control)

# traceplot
traceplot(rt_control)

# plot samples
plot_samples(rt_control)


## test group fit --------------------------------------------------------
df_test <- df_correct[df_correct$group == "test", ]

n <- nrow(df_test)
m <- length(unique(df_test$subject))
rt <- df_test$rt
s <- df_test$subject

rt_test <- b_reaction_time(n = n, m = m, rt = rt, s = s)

# summary
summary(rt_test)

# check fits
plot_fit(rt_test)

# traceplot
traceplot(rt_test)

# plot samples
plot_samples(rt_test)


## compare two groups  ---------------------------------------------------
# difference summary
compare(rt_control, fit2 = rt_test)

# difference summary, compare only mu parameter (or lambda)
compare(rt_control, fit2 = rt_test, par = "mu")

# difference summary with rope
compare(rt_control, fit2 = rt_test, rope = 0.1)

# difference plot
plot_difference(rt_control, fit2 = rt_test)

# difference plot with rope, custom bins of mu parameter
plot_difference(rt_control, fit2 = rt_test, rope = 0.1, bins = 10, par = "mu")

# samples plot
plot_samples(rt_control, fit2 = rt_test)

# plot samples only for lambda prameter (or mu)
plot_samples(rt_control, fit2 = rt_test, par = "lambda")

# compare distributions
compare_distributions(rt_control, fit2 = rt_test, rope = 0.1)

# compare distributions with rope
compare_distributions(rt_control, fit2 = rt_test)

# plot distributions
plot_distributions(rt_control, fit2 = rt_test)

# plot distributions difference
plot_distributions_difference(rt_control, fit2 = rt_test)

# plot distributions difference
plot_distributions_difference(rt_control, fit2 = rt_test, par = "mu")

# plot distributions difference with rope and custom bins
plot_distributions_difference(rt_control, fit2 = rt_test, rope = 0.1, bins = 10)



### SUCCESS RATE - test group congruent vs incongruent  ------------------
df_congruent <- df[df$group == "test" & df$congruency == "congruent", ]

df_incongruent <- df[df$group == "test" & df$congruency == "incongruent", ]

## congruent fit ---------------------------------------------------------
n <- nrow(df_congruent)
m <- length(unique(df_congruent$subject))
r <- df_congruent$result_numeric
s <- df_congruent$subject

s_congruent <- b_success_rate(n = n, m = m, r = r, s = s)

# summary
summary(s_congruent)

# check fits
plot_fit(s_congruent)

# traceplot
traceplot(s_congruent)

# plot samples
plot_samples(s_congruent)


## incongruent fit -------------------------------------------------------
n <- nrow(df_incongruent)
m <- length(unique(df_incongruent$subject))
r <- df_incongruent$result_numeric
s <- df_incongruent$subject

s_incongruent <- b_success_rate(n = n, m = m, r = r, s = s)

# summary
summary(s_incongruent)

# check fits
plot_fit(s_incongruent)

# traceplot
traceplot(s_incongruent)

# plot samples
plot_samples(s_incongruent)


## comparison ------------------------------------------------------------
# compare
compare(s_congruent, fit2 = s_incongruent)

# compare with rope
compare(s_congruent, fit2 = s_incongruent, rope = 0.05)

# difference plot
plot_difference(s_congruent, fit2 = s_incongruent)

# difference plot with rope and custom bins
plot_difference(s_congruent, fit2 = s_incongruent, rope = 0.05, bins = 10)

# plot samples for both groups
plot_samples(s_congruent, fit2 = s_incongruent)

# compare distributions
compare_distributions(s_congruent, fit2 = s_incongruent)

# distributions plost
plot_distributions(s_congruent, fit2 = s_incongruent)

# plot distributions difference
plot_distributions_difference(s_congruent, fit2 = s_incongruent)

# plot distributions difference with custom bins
plot_distributions_difference(s_congruent, fit2 = s_incongruent, bins = 10)
