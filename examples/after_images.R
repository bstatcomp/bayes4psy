# libs
library(EasyBayes)
library(dplyr)
library(rstan)

## data wrangling --------------------------------------------------------
# load data
df <- read.table("./examples/data/after_images.csv", sep="\t", header=TRUE)

# stimuli to indexes
stimuli_names <- unique(df$stimuli)
indexes <- 1:length(stimuli_names)
df$stimuli_index <- as.integer(factor(df$stimuli, levels=stimuli_names, labels=indexes))


## colour analysis -------------------------------------------------------
n <- nrow(df) # number of measurements
m <- length(unique(df$stimuli)) # number of different stimuili
stimuli <- df$stimuli_index
r <- df$r
g <- df$g
b <- df$b

# cast to hsv (do this inside lib late ron)
df[c("h", "s", "v")] <- with(df, t(rgb2hsv(r, g, b, maxColorValue=255)))
df$h <- df$h * 2 * pi

h <- df$h
s <- df$s
v <- df$v

# fit
stan_data <- list(n = n,
                  m = m,
                  stimuli = stimuli,
                  r = r,
                  g = g,
                  b = b,
                  h = h,
                  s = s,
                  v = v)

model <- stan_model(file='./examples/colours.stan')

fit <- sampling(model,
                  data = stan_data,
                  warmup = 200,
                  iter = 400,
                  chains = 1)

extract <- extract(fit)

plot_trace(fit, pars=c("mu_h"), inc_warmup=TRUE)


## plot fit --------------------------------------------------------------

library(cowplot)
library(circular)


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
