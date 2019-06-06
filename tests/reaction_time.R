library(bayes4psy)

# priors
mu_prior <- b_prior(family="normal", pars=c(0, 100))
sigma_prior <- b_prior(family="uniform", pars=c(0, 500))
lambda_prior <- b_prior(family="uniform", pars=c(0, 100))

# attach priors to relevant parameters
priors <- list(c("mu_m", mu_prior),
               c("sigma_m", sigma_prior),
               c("mu_s", sigma_prior),
               c("sigma_s", sigma_prior),
               c("mu_l", lambda_prior),
               c("sigma_l", sigma_prior))


# subjects
s <- rep(1:5, 40)

# generate data and fit
rt1 <- emg::remg(200, mu=10, sigma=1, lambda=0.2)
fit1 <- b_reaction_time(t=rt1, s=s, priors=priors, chains=1)

rt2 <- emg::remg(200, mu=10, sigma=2, lambda=0.1)
fit2 <- b_reaction_time(t=rt2, s=s, priors=priors, chains=1)

rt3 <- emg::remg(200, mu=20, sigma=2, lambda=0.05)
fit3 <- b_reaction_time(t=rt3, s=s, priors=priors, chains=1)

rt4 <- emg::remg(200, mu=15, sigma=2, lambda=0.1)
fit4 <- b_reaction_time(t=rt4, s=s, priors=priors, chains=1)

#

object <- fit1
fits <- list(fit2, fit3, fit4)
arguments <- list()
arguments$fits <- fits


plot_samples(fit1)
plot_samples(fit1, fit2=fit2)
plot_samples(fit1, fits=fits)

plot_samples_difference(fit1, fits=fits)
plot_distributions_difference(fit1, fits=fits)
