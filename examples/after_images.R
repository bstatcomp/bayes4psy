# libs
library(EasyBayes)
library(dplyr)
library(rstan)

# build model
model_vm <- stan_model(file = 'colors.stan')

## data wrangling --------------------------------------------------------
# load data
df <- read.table("../examples/data/after_images.csv", sep="\t", header=TRUE)


## colour analysis -------------------------------------------------------
n <- nrow(df) # number of measurements
r <- df$r
g <- df$g
b <- df$b

# cast to hsv (TODO: this goes inside the b_colors fitter later on)
df[c("h", "s", "v")] <- with(df, t(rgb2hsv(r, g, b, maxColorValue=255)))
df$h <- df$h * 2 * pi

h <- df$h
s <- df$s
v <- df$v

# fit
stan_data <- list(n = n,
                  r = r,
                  g = g,
                  b = b,
                  h = h,
                  s = s,
                  v = v)

model <- stan_model(file="../examples/colors.stan")

fit <- sampling(model,
                  data = stan_data,
                  warmup = 200,
                  iter = 400,
                  chains = 1)

extract <- extract(fit)

plot_trace(fit, pars=c("mu_h"), inc_warmup=TRUE)

## summary ---------------------------------------------------------------
# get means
mu_r <- mean(object@extract$mu_r)
sigma_r <- mean(object@extract$sigma_r)
mu_g <- mean(object@extract$mu_g)
sigma_g <- mean(object@extract$sigma_g)
mu_b <- mean(object@extract$mu_b)
sigma_b <- mean(object@extract$sigma_b)
mu_h <- mean(object@extract$mu_h)
kappa_h <- mean(object@extract$kappa_h)
mu_s <- mean(object@extract$mu_s)
sigma_s <- mean(object@extract$sigma_s)
mu_v <- mean(object@extract$mu_v)
sigma_v <- mean(object@extract$sigma_v)

# hdi
mu_r_hdi <- mcmc_hdi(object@extract$mu_r)
sigma_r_hdi <- mcmc_hdi(object@extract$sigma_r)
mu_g_hdi <- mcmc_hdi(object@extract$mu_g)
sigma_g_hdi <- mcmc_hdi(object@extract$sigma_g)
mu_b_hdi <- mcmc_hdi(object@extract$mu_b)
sigma_b_hdi <- mcmc_hdi(object@extract$sigma_b)
mu_h_hdi <- mcmc_hdi(object@extract$mu_h)
kappa_h_hdi <- mcmc_hdi(object@extract$kappa_h)
mu_s_hdi <- mcmc_hdi(object@extract$mu_s)
sigma_s_hdi <- mcmc_hdi(object@extract$sigma_s)
mu_v_hdi <- mcmc_hdi(object@extract$mu_v)
sigma_v_hdi <- mcmc_hdi(object@extract$sigma_v)

# print)
cat(sprintf("mu_r: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            mu_r, mcmcse::mcse(object@extract$mu_r)$se, mu_r_hdi[1], mu_r_hdi[2]))
cat(sprintf("sigma_r: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            sigma_r, mcmcse::mcse(object@extract$sigma_r)$se, sigma_r_hdi[1], sigma_r_hdi[2]))
cat(sprintf("mu_g: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            mu_g, mcmcse::mcse(object@extract$mu_g)$se, mu_g_hdi[1], mu_g_hdi[2]))
cat(sprintf("sigma_g: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            sigma_g, mcmcse::mcse(object@extract$sigma_g)$se, sigma_g_hdi[1], sigma_g_hdi[2]))
cat(sprintf("mu_b: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            mu_b, mcmcse::mcse(object@extract$mu_b)$se, mu_b_hdi[1], mu_b_hdi[2]))
cat(sprintf("sigma_b: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            sigma_b, mcmcse::mcse(object@extract$sigma_b)$se, sigma_b_hdi[1], sigma_b_hdi[2]))
cat(sprintf("mu_h: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            mu_h, mcmcse::mcse(object@extract$mu_h)$se, mu_h_hdi[1], mu_h_hdi[2]))
cat(sprintf("kappa_h: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            kappa_h, mcmcse::mcse(object@extract$kappa_h)$se, kappa_h_hdi[1], kappa_hhdi[2]))
cat(sprintf("mu_s: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            mu_s, mcmcse::mcse(object@extract$mu_s)$se, mu_s_hdi[1], mu_s_hdi[2]))
cat(sprintf("sigma_s: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            sigma_s, mcmcse::mcse(object@extract$sigma_s)$se, sigma_s_hdi[1], sigma_s_hdi[2]))
cat(sprintf("mu_v: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            mu_v, mcmcse::mcse(object@extract$mu_v)$se, mu_v_hdi[1], mu_v_hdi[2]))
cat(sprintf("sigma_v: %.2f +/- %.5f, 95%% HDI: [%.2f, %.2f]\n",
            sigma_v, mcmcse::mcse(object@extract$sigma_v)$se, sigma_v_hdi[1], sigma_v_hdi[2]))

# add stimuli data
colors <- c("cyan", "magenta", "blue", "yellow", "green", "red")
df_stimuli <- expand.grid(r_s = c(255, 0), g_s = c(255, 0), b_s = c(255, 0))[c(-1, -8), ] %>%
  mutate(stimuli = factor(colors), levels = levels(colors)) %>%
  arrange(stimuli)
df_stimuli[c("h_s", "s_s", "v_s")] <- with(df_stimuli, t(rgb2hsv(r_s, g_s, b_s, maxColorValue = 255)))
# merge stimuli data with measurements
df2 <- inner_join(df, df_stimuli)




## plot fit --------------------------------------------------------------



df_data <- select(df, stimuli, r, g, b, h, s, v)
colours <- c("red", "green", "blue", "grey50", "grey50", "grey50")

stimuli_names <- unique(df$stimuli)

plotlist <- list()
plotIndex <- 1

n <- 1000

for (i in 1:m) {
  stimuli_name <- stimuli_names[i]

  for (j in 1:6) {

    # normal distribution for all params except hue
    if (j != 4) {
      # data
      data_mean <- mean(df_data[df_data$stimuli == stimuli_name, ][, j+1])

      mu <- extract[2*j-1][[1]]
      mu <- mu[,i]

      x_min <- 0
      if (j <= 3) {
        x_max <- 255
      } else {
        x_max <- 1
      }

      # plot
      data_plot <- data.frame(value=mu)

      #graph <-
      ggplot(data=df_x, aes(x=value)) +
        geom_density(data=data_plot, aes(x=value), fill=colours[j], alpha=0.4, color=NA) +
        xlab("value") +
        ylab("density") +
        xlim(0, x_max)

    } else {
      data_mean <- mean(circular(df_data[df_data$stimuli == stimuli_name, ][, j+1]))

      mu <- extract[2*j-1][[1]]
      mu <- mu[,i]

      # plot
      data_plot <- data.frame(value=mu)

      # TODO x axis in PI units
      #graph <-
      ggplot(data=df_x, aes(x=value)) +
        geom_density(data=data_plot, aes(x=value), fill=colours[j], alpha=0.4, color=NA) +
        xlab("value") +
        ylab("density") +
        xlim(0, 2*pi)
    }

    plotlist[[plotIndex]] <- graph

    plotIndex <- plotIndex + 1
  }
}




cowplot::plot_grid(plotlist=plotlist, ncol=6, nrow=m, scale=0.9)

mu <- pi
kappa <- 1

x <- seq(-pi,pi,0.01)
y <- (exp(kappa * cos(x - mu))) / (2 * pi * besselI(kappa, 0))

plot(x, y)
