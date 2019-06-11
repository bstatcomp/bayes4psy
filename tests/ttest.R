library(bayes4psy)

# priors
mu_prior <- b_prior(family="normal", pars=c(0, 1000))
sigma_prior <- b_prior(family="uniform", pars=c(0, 500))

# attach priors to relevant parameters
priors <- list(c("mu", mu_prior),
               c("sigma", sigma_prior))


# generate data and fit
data1 <- rnorm(20, mean=150, sd=20)
fit1 <- b_ttest(data=data1, priors=priors, chains=1)

data2 <- rnorm(20, mean=200, sd=20)
fit2 <- b_ttest(data=data2, priors=priors, chains=1)

data3 <- rnorm(20, mean=150, sd=40)
fit3 <- b_ttest(data=data3, priors=priors, chains=1)

data4 <- rnorm(20, mean=50, sd=10)
fit4 <- b_ttest(data=data4, priors=priors, chains=1)

# fit list
fit_list <- list(fit2, fit3, fit4)

# summary
summary(fit1)

# print(`ttest_class`)
print(fit1)
fit1

# show(`ttest_class`)
show(fit1)

# get_parameters(`ttest_class`)
parameters <- get_parameters(fit1)

# compare_means(`ttest_class`, fit2=`ttest_class`)
compare_means(fit1, fit2=fit2)

# compare_means(`ttest_class`, mu=`numeric`)
compare_means(fit1, mu=150)

# compare_means(`ttest_class`, mu=`numeric`, sigma=`numeric`)
compare_means(fit1, mu=150, sigma=20)

# compare_means(`ttest_class`, fits=`list`)
compare_means(fit1, fits=fit_list)

# plot_means_difference(`ttest_class`, fit2=`ttest_class`)
plot_means_difference(fit1, fit2=fit2)

# plot_means_difference(`ttest_class`, mu=`numeric`)
plot_means_difference(fit1, mu=150)

# plot_means_difference(`ttest_class`, fits=`list`)
plot_means_difference(fit1, fits=fit_list)

# plot_means(`ttest_class`)
plot_means(fit1)

# plot_means(`ttest_class`, fit2=`ttest_class`)
plot_means(fit1, fit2=fit2)

# plot_means(`ttest_class`, mu=`numeric`)
plot_means(fit1, mu=150)

# plot_means(`ttest_class`, fits=`list`)
plot_means(fit1, fits=fit_list)

# compare_distributions(`ttest_class`, fit2=`ttest_class`)
compare_distributions(fit1, fit2=fit2)

# compare_distributions(`ttest_class`, mu=`numeric`)
compare_distributions(fit1, mu=150)

# compare_distributions(`ttest_class`, mu=`numeric`, sigma=`numeric`)
compare_distributions(fit1, mu=150, sigma=20)

# compare_distributions(`ttest_class`, fits=`list`)
compare_distributions(fit1, fits=fit_list)

# plot_distributions(`ttest_class`)
plot_distributions(fit1)

# plot_distributions(`ttest_class`, fit2=`ttest_class`)
plot_distributions(fit1, fit2=fit2)

# plot_distributions(`ttest_class`, mu=`numeric`)
plot_distributions(fit1, mu=150)

# plot_distributions(`ttest_class`, mu=`numeric`, sigma=`numeric`)
plot_distributions(fit1, mu=150, sigma=20)

# plot_distributions(`ttest_class`, fits=`list`): a visualization of multiple fitted distributions.
plot_distributions(fit1, fits=fit_list)

# plot_distributions_difference(`ttest_class`, fit2=`ttest_class`)
plot_distributions_difference(fit1, fit2=fit2)

# plot_distributions_difference(`ttest_class`, mu=`numeric`)
plot_distributions_difference(fit1, mu=150)

# plot_distributions_difference(`ttest_class`, mu=`numeric`, sigma=`numeric`)
plot_distributions_difference(fit1, mu=150, sigma=20)

# plot_distributions_difference(`ttest_class`, fits=`list`)
plot_distributions_difference(fit1, fits=fit_list)

# plot_fit(`ttest_class`)
plot_fit(fit1)

# plot_trace(`ttest_class`)
plot_trace(fit1)
