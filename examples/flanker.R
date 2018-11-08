# libs
library(EasyBayes)

# load data --------------------------------------------------------------------
df <- read.table("./examples/data/flanker.csv", sep = "\t")

# map correct/incorrect/timeout to 1/0
df$result_numeric <- 0
df[df$result == "correct", ]$result_numeric <- 1

### REACTION TIMES
# test vs control (correct and no timeout only)
df_correct <- df[df$result == "correct", ]


## control group
df_control <- df_correct[df_correct$group == "control", ]

# subject indexes range on 22..45 cast to 1..23
df_control$subject <- df_control$subject - 21

n <- nrow(df_control)
m <- length(unique(df_control$subject))
rt <- df_control$rt
s <- df_control$subject

rt_control <- b_reaction_times(n = n, m = m, rt = rt, s = s)

#summary
summary(rt_control)

# check fits
plot_fit(rt_control)

# traceplot
traceplot(rt_control)


## test group
df_test <- df_correct[df_correct$group == "test", ]

n <- nrow(df_test)
m <- length(unique(df_test$subject))
rt <- df_test$rt
s <- df_test$subject

rt_test <- b_reaction_times(n = n, m = m, rt = rt, s = s)

#summary
summary(rt_test)

# check fits
plot_fit(rt_test)

# traceplot
traceplot(rt_test)


## compare two groups
# difference summary
compare(rt_control, rt_test)

# difference plot
plot_difference(rt_control, rt_test)

# comparison plot
plot_comparison(rt_control, rt_test)

# compare distributions
compare_distributions(rt_control, rt_test)

# plot distributions
plot_distributions(rt_control, rt_test)

# plot distributions difference
plot_distributions_difference(rt_control, rt_test)


# TODO: PER SUBJECT COMPARISON
# TODO: SUCCESSES
#e1 <- object1@extract$a / (object1@extract$a + object1@extract$b)
#e2 <- object2@extract$a / (object2@extract$a + object2@extract$b)
