# libs
library(bayes4psy)

## data wrangling -------------------------------------------------------------
# load data
data <- read.table("../examples/data/flanker.csv", sep="\t", header=TRUE)

## reaction time analysis - test vs control group -----------------------------
# analyse only correct answers
correct <- data[data$result == "correct", ]

## fit reaction times model to control and test groups fit --------------------
control_rt <- correct[correct$group == "control", ]
test_rt <- correct[correct$group == "test", ]

# control group subject indexes range is 22..45 cast it to 1..23
# test group indexes are OK
control_rt$subject <- control_rt$subject - 21

# fit, use only 1 chain to speed up the process
# this is only a demonstration
# more chains or a longer chian would be needed for proper analysis
rt_control_fit <- b_reaction_time(t=control_rt$rt, s=control_rt$subject,
                                  chain=1, warmup=500, iter=2000)

rt_test_fit <- b_reaction_time(t=test_rt$rt, s=test_rt$subject,
                                  chain=1, warmup=500, iter=2000)

# plot trace
plot_trace(rt_control_fit)
plot_trace(rt_test_fit)

# check fit (Rhat and n_eff)
print(rt_control_fit)
print(rt_test_fit)

# check fits
plot_fit(rt_control_fit)
plot_fit(rt_control_fit, subjects=TRUE)
plot_fit(rt_test_fit)
plot_fit(rt_test_fit, subjects=TRUE)


## analysis of reaction times between control and test group ------------------
# set rope (region of practical equivalence) interval to +/- 10ms
rope <- 0.01

# which one is
rt_control_test <- compare_means(rt_control_fit, fit2=rt_test_fit, rope=rope)

# difference plot
plot_means_difference(rt_control_fit, fit2=rt_test_fit, rope=rope)

# visual comparsion of mean difference
plot_means(rt_control_fit, fit2=rt_test_fit)


## sucess rate analysis - test vs control group -------------------------------
# map correct/incorrect/timeout to 1 (success) or 0 (fail)
data$result_numeric <- 0
data[data$result == "correct", ]$result_numeric <- 1

# split to control and test groups
control_sr <- data[data$group == "control", ]
test_sr <- data[data$group == "test", ]

# control group subject indexes range is 22..45 cast it to 1..23
# test group indexes are OK
control_sr$subject <- control_sr$subject - 21

# priors
p_prior <- b_prior(family="beta", pars=c(1, 1))
tau_prior <- b_prior(family="uniform", pars=c(0, 500))

# attach priors to relevant parameters
priors <- list(c("p", p_prior),
               c("tau", tau_prior))

# fit
sr_control_fit <- b_success_rate(r=control_sr$result_numeric,
                                 s=control_sr$subject,
                                 priors=priors)
sr_test_fit <- b_success_rate(r=test_sr$result_numeric,
                              s=test_sr$subject,
                              priors=priors)

# plot trace
plot_trace(sr_control_fit)
plot_trace(sr_test_fit)

# check fit (Rhat and n_eff)
print(sr_control_fit)
print(sr_test_fit)

# check fits
plot_fit(sr_control_fit)
plot_fit(sr_control_fit, subjects=TRUE)
plot_fit(sr_test_fit)
plot_fit(sr_test_fit, subjects=TRUE)


## analysis of sucess rate between control and test group ------------------
# set rope (region of practical equivalence) interval to +/- 10ms
# which one is
sr_control_test <- compare_means(sr_control_fit, fit2=sr_test_fit)

# difference plot
plot_means_difference(sr_control_fit, fit2=sr_test_fit)

# visual comparsion of mean difference
plot_means(sr_control_fit, fit2=sr_test_fit)
