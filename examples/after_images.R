# libs
library(EasyBayes)
library(dplyr)
library(rstan)

## dummy s4 class
color_class <- setClass(
  "color_class",
  slots = c(
    extract  = "list",
    fit = "stanfit",
    data = "list"
  )
)

# build model
model <- stan_model(file = 'src/stan_files/color.stan')

## data wrangling --------------------------------------------------------
# load data
df_all <- read.table("../examples/data/after_images.csv", sep="\t", header=TRUE)

df_blue <- df_all %>% filter(stimuli == "blue")
df <- df_blue

## BLUE
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

fit <- sampling(model,
                  data = stan_data,
                  warmup = 2000,
                  iter = 3000,
                  chains = 1)

extract <- extract(fit)

object <- new("color_class", extract=extract, fit=fit, data=stan_data)


## SECOND FIT
## RED
df_red <- df_all %>% filter(stimuli == "red")
df <- df_red

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

fit <- sampling(model,
                data = stan_data,
                warmup = 2000,
                iter = 3000,
                chains = 1)

extract <- extract(fit)

fit2 <- new("color_class", extract=extract, fit=fit, data=stan_data)




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


## TODO FOR EXAMPLE

# add stimuli data
colors <- c("cyan", "magenta", "blue", "yellow", "green", "red")
df_stimuli <- expand.grid(r_s = c(255, 0), g_s = c(255, 0), b_s = c(255, 0))[c(-1, -8), ] %>%
  mutate(stimuli = factor(colors), levels = levels(colors)) %>%
  arrange(stimuli)
df_stimuli[c("h_s", "s_s", "v_s")] <- with(df_stimuli, t(rgb2hsv(r_s, g_s, b_s, maxColorValue = 255)))
# merge stimuli data with measurements
df2 <- inner_join(df, df_stimuli)

