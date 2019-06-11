library(bayes4psy)

# priors
p_prior <- b_prior(family="beta", pars=c(1, 1))
tau_prior <- b_prior(family="uniform", pars=c(0, 500))

# attach priors to relevant parameters
priors <- list(c("p", p_prior),
               c("tau", tau_prior))

# subjects
s <- rep(1:5, 20)

# generate data and fit
data1 <- rbinom(100, size=1, prob=0.6)
fit1 <- b_success_rate(r=data1, s=s, priors=priors, chains=1)

data2 <- rbinom(100, size=1, prob=0.1)
fit2 <- b_success_rate(r=data2, s=s, priors=priors, chains=1)

data3 <- rbinom(100, size=1, prob=0.5)
fit3 <- b_success_rate(r=data3, s=s, priors=priors, chains=1)

data4 <- rbinom(100, size=1, prob=0.9)
fit4 <- b_success_rate(r=data4, s=s, priors=priors, chains=1)

# fit list
fit_list <- list(fit2, fit3, fit4)

# summary
summary(fit1)

# print(`success_rate_class`)
print(fit1)
fit1

# show(`success_rate_class`)
show(fit1)

# get_parameters(`success_rate_class`)
parameters <- get_parameters(fit1)

# get_subject_parameters(`success_rate_class`)
subject_parameters <- get_subject_parameters(fit1)

# compare_means(`success_rate_class`, fit2=`success_rate_class`)
compare_means(fit1, fit2=fit2)

# compare_means(`success_rate_class`, fits=`list`)
compare_means(fit1, fits=fit_list)

# plot_means_difference(`success_rate_class`, fit2=`success_rate_class`)
plot_means_difference(fit1, fit2=fit2)

# plot_means_difference(`success_rate_class`, fits=`list`)
plot_means_difference(fit1, fits=fit_list)

# plot_means(`success_rate_class`)
plot_means(fit1)

# plot_means(`success_rate_class`, fit2=`success_rate_class`)
plot_means(fit1, fit2=fit2)

# plot_means(`success_rate_class`, fits=`list`)
plot_means(fit1, fits=fit_list)

# compare_distributions(`success_rate_class`, fit2=`success_rate_class`)
compare_distributions(fit1, fit2=fit2)

# compare_distributions(`success_rate_class`, fits=`list`)
compare_distributions(fit1, fits=fit_list)

# plot_distributions(`success_rate_class`)
plot_distributions(fit1)

# plot_distributions(`success_rate_class`, fit2=`success_rate_class`)
plot_distributions(fit1, fit2=fit2)

# plot_distributions(`success_rate_class`, fits=`list`): a visualization of multiple fitted distributions.
plot_distributions(fit1, fits=fit_list)

# plot_distributions_difference(`success_rate_class`, fit2=`success_rate_class`)
plot_distributions_difference(fit1, fit2=fit2)

# plot_distributions_difference(`success_rate_class`, fits=`list`)
plot_distributions_difference(fit1, fits=fit_list)

# plot_fit(`success_rate_class`)
plot_fit(fit1)

# plot_fit(`success_rate_class`, subjects='boolean')
plot_fit(fit1, subjects=TRUE)

# plot_trace(`success_rate_class`)
plot_trace(fit1)
