library(ggplot2)
library(rstan)
library(reshape2)
library(mcmcse)
library(emg)
library(plyr)

# stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data --------------------------------------------------------------------
full_dat <- read.table("./examples/data/flanker.txt", sep = "\t", h = T)
full_dat <- full_dat[,c(1,2,3,4,5,7)]
full_dat$subject <- as.numeric(as.factor(full_dat$subject))

# build or load model ----------------------------------------------------------
model_rt <- stan_model(file = "./src/stan_files/flanker_rt.stan")
model_s <- stan_model(file = "./src/stan_files/flanker_s.stan")

# function for fitting data to model -------------------------------------------
fit_to_model <- function(data) {
  X <- dcast(tmp, subject ~ num, value.var = "rt", fun.aggregate = mean)[,-1]
  stan_data <- list(n = nrow(X), m = ncol(X), T = X)

  output <- sampling(model_rt,
                     data = stan_data,
                     iter = 1000,
                     warmup = 500,
                     chains = 1,
                     control = list(adapt_delta = 0.99),
                     seed = 0)

  return(output)
}


# function for comparing fit with subject data ---------------------------------
compare_subjects_emg <- function(dat, smp) {
  tmp <- NULL
  subjects <- unique(dat$subject)
  n <- length(subjects)

  for (i in 1:n) {
    df <- data.frame(x = seq(0, 2, 0.01),
                     subject = subjects[i],
                     variable = "posterior mean",
                     y = demg(seq(0, 2, 0.01),
                              mu = mean(smp[,i]),
                              lambda = mean(smp[,2 * n + i]),
                              sigma = mean(smp[,n + i])))

    tmp <- rbind(tmp, df)
  }

  # density per subject
  p <- ggplot(dat, aes(x = rt, colour = variable)) +
    geom_density() +
    facet_wrap(~ subject, ncol = 6) +
    geom_line(data = tmp, aes(x = x, y = y))

  return(p)
}


# function for comparing two emg fits ------------------------------------------
compare_emgs <- function(samples1, samples2) {
  x <- seq(0, 2, 0.01)
  y1 <- demg(x,
             mu = mean(samples1$mu_m),
             sigma = mean(sqrt(samples1$ss_m)),
             lambda = mean(samples1$mu_l))
  y2 <- demg(x,
             mu = mean(samples2$mu_m),
             sigma = mean(sqrt(samples2$ss_m)),
             lambda = mean(samples2$mu_l))

  df <- data.frame(x = x, left = y1, right = y2)
  df <- melt(df, id=c("x"))

  p <- ggplot(df, aes(x = x, y = value, colour = variable)) +
    geom_line(size = 2) +
    theme_minimal() +
    scale_colour_brewer(type = "qual")

  return(p)
}

# Analysis #1 - left vs right control ------------------------------------------
dat <- full_dat[full_dat$group == "CON",]

# left
dat_left <- dat[dat$direction == "left", c(1,6)]
# remove subjects with no session 2 results
dat_left <- ddply(dat_left, "subject", function(x) {if (nrow(x) == 96) x else NULL})
dat_left$num <- 1:96
output_left <- fit_to_model(dat_left)
extract_left <- extract(output_left, permuted = F)
smp_left <- data.frame(extract_left[,1,])

# right
dat_right <- dat[dat$direction == "right", c(1,6)]
# remove subjects with no session 2 results
dat_right <- ddply(dat_right, "subject", function(x) {if (nrow(x) == 96) x else NULL})
dat_right$num <- 1:96
output_right <- fit_to_model(dat_right)
extract_right <- extract(output_right, permuted = F)
smp_right <- data.frame(extract_right[,1,])


# visual inspection & comparison -----------------------------------------------

# diagnostics
traceplot(output_left, pars = c("mu_m", "ss_m", "mu_l", "ss_l"))

# compare left vs right means
p1 <- compare_emgs(smp_left, smp_right)
plot(p1)

# per subject plot
dat_left$variable = "left"
p2 <- compare_subjects_emg(dat_left, smp_left)
plot(p2)


# sample means vs estimated mu vs estimated mu + 1/lambda
act <- tapply(tmpX$rt, tmpX$subject, mean)
mus <- colMeans(smp_X[(1:n)])
lls <- colMeans(smp_X[2*n + (1:n)])
plot(act, mus, xlim = c(0, 1), ylim = c(0, 1))
segments(0,0,100,100)
plot(act, mus + 1/lls, xlim = c(0, 1), ylim = c(0, 1))
segments(0,0,100,100)

# Hypothesis: group A mean < group B mean
x <- smp_A$mu_m < smp_B$mu_m
mcse(x)

x <- smp_A$mu_m + 1/smp_A$mu_l < smp_B$mu_m + 1/smp_B$mu_l
mcse(x)

# t-test
z <- tapply(dat$rt, list(dat$subject, dat$congruency), mean)
print(z)
print(t.test(z[,2] - z[,1], alt = "greater"))
