library(bayes4psy)

# priors
mu_prior <- b_prior(family="normal", pars=c(0, 100))
sigma_prior <- b_prior(family="uniform", pars=c(0, 500))

# attach priors to relevant parameters
priors <- list(c("mu_a", mu_prior),
               c("sigma_a", sigma_prior),
               c("mu_b", mu_prior),
               c("sigma_b", sigma_prior),
               c("mu_s", sigma_prior),
               c("sigma_s", sigma_prior))


# generate data and fit
x <- vector()
y <- vector()
s <- vector()
for (i in 1:5) {
  x <- c(x, rep(1:10, 2))
  y <- c(y, rnorm(20, mean=1:10, sd=2))
  s <- c(s, rep(i, 20))
}

fit1 <- b_linear(x=x, y=y, s=s, priors=priors, chains=1)

fit2 <- b_linear(x=x, y=-2*y, s=s, priors=priors, chains=1)

# summary
summary(fit1)

# print(`linear_class`)
print(fit1)
fit1

# show(`linear_class`)
show(fit1)

# get_parameters(`linear_class`)
parameters <- get_parameters(fit1)

# get_subject_parameters(`linear_class`)
subject_parameters <- get_subject_parameters(fit1)

# compare_means(`linear_class`, fit2=`linear_class`)
compare_means(fit1, fit2=fit2)

# plot_means_difference(`linear_class`, fit2=`linear_class`)
plot_means_difference(fit1, fit2=fit2)

# plot_means_difference(`linear_class`, fit2=`linear_class`, par='intercept/slope')
plot_means_difference(fit1, fit2=fit2, par="intercept")

# plot_means(`linear_class`)
plot_means(fit1)

# plot_means(`linear_class`, fit2=`linear_class`)
plot_means(fit1, fit2=fit2)

# plot_means(`linear_class`, fit2=`linear_class`, par='intercept/slope')
plot_means(fit1, fit2=fit2, par="intercept")

# compare_distributions(`linear_class`, fit2=`linear_class`)
compare_distributions(fit1, fit2=fit2)

# plot_distributions(`linear_class`)
plot_distributions(fit1)

# plot_distributions(`linear_class`, fit2=`linear_class`)
plot_distributions(fit1, fit2=fit2)

# plot_distributions_difference(`linear_class`, fit2=`linear_class`)
plot_distributions_difference(fit1, fit2=fit2)

# plot_distributions_difference(`linear_class`, fit2=`linear_class`, par='intercept/slope')
plot_distributions_difference(fit1, fit2=fit2, par="intercept")

# plot_fit(`linear_class`)
plot_fit(fit1)

# plot_fit(`linear_class`, subjects='boolean')
plot_fit(fit1, subjects=TRUE)

# plot_trace(`linear_class`)
plot_trace(fit1)
