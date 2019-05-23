library(EasyBayes)

s <- rep(1:5, 20)

rt <- emg::remg(100, mu=1, sigma=0.5, lambda=2)
fit1 <- b_reaction_time(t=rt, s=s, chains=1)

rt <- emg::remg(100, mu=2, sigma=0.5, lambda=2)
fit2 <- b_reaction_time(t=rt, s=s, chains=1)

rt <- emg::remg(100, mu=1, sigma=0.5, lambda=0.5)
fit3 <- b_reaction_time(t=rt, s=s, chains=1)

rt <- emg::remg(100, mu=1.5, sigma=0.1, lambda=2)
fit4 <- b_reaction_time(t=rt, s=s, chains=1)

object <- fit1
arguments <- list()
arguments$fits <- list(fit2, fit3, fit4)
