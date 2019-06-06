library(EasyBayes)

s <- rep(1:5, 40)

rt <- emg::remg(200, mu=10, sigma=1, lambda=0.2)
fit1 <- b_reaction_time(t=rt, s=s, chains=1)

rt <- emg::remg(200, mu=10, sigma=2, lambda=0.1)
fit2 <- b_reaction_time(t=rt, s=s, chains=1)

rt <- emg::remg(200, mu=20, sigma=2, lambda=0.05)
fit3 <- b_reaction_time(t=rt, s=s, chains=1)

rt <- emg::remg(200, mu=15, sigma=2, lambda=0.1)
fit4 <- b_reaction_time(t=rt, s=s, chains=1)

object <- fit1
fits <- list(fit2, fit3, fit4)
arguments <- list()
arguments$fits <- fits


plot_samples(fit1)
plot_samples(fit1, fit2=fit2)
plot_samples(fit1, fits=fits)

plot_samples_difference(fit1, fits=fits)
plot_distributions_difference(fit1, fits=fits)
