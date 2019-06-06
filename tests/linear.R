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
