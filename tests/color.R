library(bayes4psy)

# priors for rgb
mu_prior <- b_prior(family="uniform", pars=c(0, 255))
sigma_prior <- b_prior(family="uniform", pars=c(0, 50))

# attach priors to relevant parameters
priors_rgb <- list(c("mu_r", mu_prior),
               c("sigma_r", sigma_prior),
               c("mu_g", mu_prior),
               c("sigma_g", sigma_prior),
               c("mu_b", mu_prior),
               c("sigma_b", sigma_prior))


# generate data (rgb) and fit
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

fit1 <- b_color(colors=colors, priors=priors_rgb, chains=1)


# priors for hsv
h_prior <- b_prior(family="uniform", pars=c(0, 2*pi))
sv_prior <- b_prior(family="uniform", pars=c(0, 1))
variance_prior <- b_prior(family="uniform", pars=c(0, 2))

# attach priors to relevant parameters
priors_hsv <- list(c("mu_h", h_prior),
                   c("kappa_h", variance_prior),
                   c("mu_s", sv_prior),
                   c("sigma_s", variance_prior),
                   c("mu_v", sv_prior),
                   c("sigma_v", variance_prior))

# generate data (hsv) and fit
h <- rnorm(50, mean=2*pi/3, sd=0.5)
h[h > 2*pi] <- 2*pi
h[h < 0] <- 0

s <- rnorm(50, mean=0.9, sd=0.05)
s[s > 1] <- 1
s[s < 0] <- 0

v <- rnorm(50, mean=0.9, sd=0.1)
v[v > 1] <- 1
v[b < 0] <- 0

colors <- data.frame(h=h, s=s, v=v)

fit2 <- b_color(colors=colors, hsv=TRUE, priors=priors_hsv, chains=1)


# summary
summary(fit1)

# print(`color_class`)
print(fit1)
fit1

# show(`color_class`)
show(fit1)

# get_parameters(`color_class`)
parameters <- get_parameters(fit1)

# compare_means(`color_class`, fit2=`color_class`)
compare_means(fit1, fit2=fit2)

# plot_means_difference(`color_class`, fit2=`color_class`)
plot_means_difference(fit1, fit2=fit2)

# plot_means(`color_class`)
plot_means(fit1)

# plot_means(`color_class`, fit2=`color_class`)
plot_means(fit1, fit2=fit2)

# compare_distributions(`color_class`, fit2=`color_class`)
compare_distributions(fit1, fit2=fit2)

# plot_distributions(`color_class`)
plot_distributions(fit1)

# plot_distributions(`color_class`, fit2=`color_class`)
plot_distributions(fit1, fit2=fit2)

# plot_distributions_difference(`color_class`, fit2=`color_class`)
plot_distributions_difference(fit1, fit2=fit2)

# plot_fit(`color_class`)
plot_fit(fit1)

# plot_trace(`color_class`)
plot_trace(fit1)
