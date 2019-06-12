library(bayes4psy)

# priors
mu_prior <- b_prior(family="normal", pars=c(0, 100))
sigma_prior <- b_prior(family="uniform", pars=c(0, 500))
lambda_prior <- b_prior(family="uniform", pars=c(0.05, 5))

# attach priors to relevant parameters
priors <- list(c("mu_m", mu_prior),
               c("sigma_m", sigma_prior),
               c("mu_s", sigma_prior),
               c("sigma_s", sigma_prior),
               c("mu_l", lambda_prior),
               c("sigma_l", sigma_prior))


# subjects
s <- rep(1:5, 20)

# generate data and fit
rt1 <- emg::remg(100, mu=10, sigma=1, lambda=0.4)
fit1 <- b_reaction_time(t=rt1, s=s, priors=priors, chains=1)

rt2 <- emg::remg(100, mu=10, sigma=2, lambda=0.1)
fit2 <- b_reaction_time(t=rt2, s=s, priors=priors, chains=1)

rt3 <- emg::remg(100, mu=20, sigma=2, lambda=1)
fit3 <- b_reaction_time(t=rt3, s=s, priors=priors, chains=1)

rt4 <- emg::remg(100, mu=15, sigma=2, lambda=0.5)
fit4 <- b_reaction_time(t=rt4, s=s, priors=priors, chains=1)

# fit list
fit_list <- list(fit2, fit3, fit4)


# summary
summary(fit1)

# print(`reaction_time_class`)
print(fit1)
fit1

# show(`reaction_time_class`)
show(fit1)

# get_parameters(`reaction_time_class`)
parameters <- get_parameters(fit1)

# get_subject_parameters(`reaction_time_class`)
subject_parameters <- get_subject_parameters(fit1)

# compare_means(`reaction_time_class`, fit2=`reaction_time_class`)
compare_means(fit1, fit2=fit2)

# compare_means(`reaction_time_class`, fit2=`reaction_time_class`, par='mu/lambda")
compare_means(fit1, fit2=fit2, par="mu")

# compare_means(`reaction_time_class`, fits=`list`)
compare_means(fit1, fits=fit_list)

# plot_means_difference(`reaction_time_class`, fit2=`reaction_time_class`)
plot_means_difference(fit1, fit2=fit2)

# plot_means_difference(`reaction_time_class`, fit2=`reaction_time_class`, par='mu/lambda")
plot_means_difference(fit1, fit2=fit2, par="mu")

# plot_means_difference(`reaction_time_class`, fits=`list`)
plot_means_difference(fit1, fits=fit_list)

# plot_means(`reaction_time_class`)
plot_means(fit1)

# plot_means(`reaction_time_class`, fit2=`reaction_time_class`)
plot_means(fit1, fit2=fit1)

# plot_means(`reaction_time_class`, fit2=`reaction_time_class`, par='mu/lambda")
plot_means(fit1, fit2=fit2, par="mu")

# plot_means(`reaction_time_class`, fits=`list`)
plot_means(fit1, fits=fit_list)

# compare_distributions(`reaction_time_class`, fit2=`reaction_time_class`)
compare_distributions(fit1, fit2=fit2)

# compare_distributions(`reaction_time_class`, fits=`list`)
compare_distributions(fit1, fits=fit_list)

# plot_distributions(`reaction_time_class`)
plot_distributions(fit1)

# plot_distributions(`reaction_time_class`, fit2=`reaction_time_class`)
plot_distributions(fit1, fit2=fit2)

# plot_distributions(`reaction_time_class`, fits=`list`): a visualization of multiple fitted distributions.
plot_distributions(fit1, fits=fit_list)

# plot_distributions_difference(`reaction_time_class`, fit2=`reaction_time_class`)
plot_distributions_difference(fit1, fit2=fit2)

# plot_distributions_difference(`reaction_time_class`, fits=`list`)
plot_distributions_difference(fit1, fits=fit_list)

# plot_fit(`reaction_time_class`)
plot_fit(fit1)

# plot_fit(`reaction_time_class`, subjects='boolean')
plot_fit(fit1, subjects=TRUE)

# plot_trace(`reaction_time_class`)
plot_trace(fit1)


