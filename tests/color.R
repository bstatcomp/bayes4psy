library(bayes4psy)

# priors
mu_prior <- b_prior(family="uniform", pars=c(0, 255))
sigma_prior <- b_prior(family="uniform", pars=c(0, 50))

# attach priors to relevant parameters
priors <- list(c("mu_r", mu_prior),
               c("sigma_r", sigma_prior),
               c("mu_g", mu_prior),
               c("sigma_g", sigma_prior),
               c("mu_b", mu_prior),
               c("sigma_b", sigma_prior))


# generate data and fit
r <- rnorm(50, mean=200, sd=20)
r[r > 255] <- 255
r[r < 0] <- 0

g <- rnorm(50, mean=20, sd=5)
g[g > 255] <- 255
g[g < 0] <- 0

b <- rnorm(50, mean=20, sd=5)
b[b > 255] <- 255
b[b < 0] <- 0

colors <- data.frame(r=r, g=g, b=b)

fit1 <- b_color(colors=colors, priors=priors, chains=1)
