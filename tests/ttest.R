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
