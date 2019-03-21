library(rstan)
library(circular)

# models
model_n <- stan_model(file = 'normal.stan')
model_vm <- stan_model(file = 'vm.stan')

# generate data
n <- 1000
h_n <- rnorm(n, 2*pi, 0.2)

# circular for von mises
h_vm <- h_n
h_vm[h_vm > 2*pi] <- h_vm[h_vm > 2*pi] - 2*pi

# fit
data_n <- list(n = n, h = h_n)
fit_n <- sampling(model_n, 
                  data = data_n,
                  warmup = 1000, 
                  iter = 5000,
                  chains = 1)
ex_n <- extract(fit_n)

mu_n <- mean(ex_n$mu)
sigma_n <- mean(ex_n$sigma)

data_vm <- list(n = n, h = h_vm)
fit_vm <- sampling(model_vm, 
                   data = data_vm,
                   warmup = 1000, 
                   iter = 5000,
                   chains = 1)
ex_vm <- extract(fit_vm)

mu_vm <- mean(ex_vm$mu)
kappa_vm <- mean(ex_vm$kappa)

# plot
m <- 10000
df_x <- data.frame(value=c(0, 3*pi))
ggplot(data=df_x, aes(x=value)) +
  stat_function(fun=dnorm, n=m, args=list(mean=mu_n, sd=sigma_n), geom="area", fill="#ff4e3f", alpha=0.4) +
  stat_function(fun=dvonmises, n=m, args=list(mu=mu_vm, kappa=kappa_vm), geom="area", fill="#3182bd", alpha=0.4)
  
  

