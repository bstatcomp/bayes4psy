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
data1 <- rbinom(100, size=1, prob=0.7)
fit1 <- b_success_rate(r=data1, s=s, priors=priors, chains=1)
