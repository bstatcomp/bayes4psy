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
model <- stan_model(file = 'colors.stan')

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



## trace plot ------------------------------------------------------------
traceplot(object@fit, pars=c("mu_r", "mu_g", "mu_b", "mu_h", "mu_s", "mu_v"), inc_warmup=TRUE)



## show ----------------------------------------------------------------
show(object@fit)


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

traceplot(fit2@fit, pars=c("mu_r", "mu_g", "mu_b", "mu_h", "mu_s", "mu_v"), inc_warmup=TRUE)



## compare ------------------------------------------------------------
rope <- 1
rope <- prepare_rope(rope)

# compare fit1, fit2
y1 <- object@extract$mu_r
y2 <- fit2@extract$mu_r
cat("\n---------- R component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_g
y2 <- fit2@extract$mu_g
cat("\n---------- G component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_b
y2 <- fit2@extract$mu_b
cat("\n---------- B component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_h
y2 <- fit2@extract$mu_h
cat("\n---------- H component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope, angular=TRUE)

y1 <- object@extract$mu_s
y2 <- fit2@extract$mu_s
cat("\n---------- S component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_v
y2 <- fit2@extract$mu_v
cat("\n---------- V component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)



# compare fit1, rgb
rgb <- c(255,0,0)
r <- rgb[1]
g <- rgb[2]
b <- rgb[3]

hsv <- rgb2hsv(r, g, b, maxColorValue=255)
h <- hsv[1]
s <- hsv[2]
v <- hsv[3]

# compare fit1, hsv
hsv <- c(0, 1, 1)
h <- hsv[1]
s <- hsv[2]
v <- hsv[3]

rgb <- hsv2rgb(hsv)
r <- rgb[1]
g <- rgb[2]
b <- rgb[3]

# compare fit1, constant
y1 <- object@extract$mu_r
y2 <- r
cat("\n---------- R component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_g
y2 <- g
cat("\n---------- G component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_b
y2 <- b
cat("\n---------- B component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_h
y2 <- h
cat("\n---------- H component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_s
y2 <- s
cat("\n---------- S component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)

y1 <- object@extract$mu_v
y2 <- v
cat("\n---------- V component ----------\n")
shared_difference(y1=y1, y2=y2, rope=rope)


## plot_difference --------------------------------------------------------
bins = 30
rope <- 1
rope <- prepare_rope(rope)

# difference between two fits
y1 <- object@extract$mu_r
y2 <- fit2@extract$mu_r
graph_r <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_g
y2 <- fit2@extract$mu_g
graph_g <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_b
y2 <- fit2@extract$mu_b
graph_b <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_h
y2 <- fit2@extract$mu_h
graph_h <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_s
y2 <- fit2@extract$mu_s
graph_s <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_v
y2 <- fit2@extract$mu_v
graph_v <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

cowplot::plot_grid(graph_r, graph_g, graph_b, graph_h, graph_s, graph_v, ncol=3, nrow=2, scale=0.9)

# difference between rgb, hsv
# compare fit1, rgb
rgb <- c(255,0,0)
r <- rgb[1]
g <- rgb[2]
b <- rgb[3]

hsv <- rgb2hsv(r, g, b, maxColorValue=255)
h <- hsv[1]
s <- hsv[2]
v <- hsv[3]

# compare fit1, hsv
hsv <- c(0, 1, 1)
h <- hsv[1]
s <- hsv[2]
v <- hsv[3]

rgb <- hsv2rgb(hsv)
r <- rgb[1]
g <- rgb[2]
b <- rgb[3]

y1 <- object@extract$mu_r
y2 <- r
graph_r <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_g
y2 <- g
graph_g <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_b
y2 <- b
graph_b <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_h
y2 <- h
graph_h <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_s
y2 <- s
graph_s <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

y1 <- object@extract$mu_v
y2 <- v
graph_v <- shared_plot_difference(y1=y1, y2=y2, rope=rope, bins=bins)

cowplot::plot_grid(graph_r, graph_g, graph_b, graph_h, graph_s, graph_v, ncol=3, nrow=2, scale=0.9)


## TODO HUE DIFFERENCE PLOT


## plot samples ----------------------------------------------------------
# TODO ALL COMPONENTS

# two fits
df <- data.frame(value=object@extract$mu_r, group="1")
df <- rbind(df, data.frame(value=fit2@extract$mu_r, group="2"))

# limits
x_min <- min(df$value)
x_max <- max(df$value)
diff <- x_max - x_min
x_min <- x_min - 0.1*diff
x_max <- x_max + 0.1*diff

graph <- ggplot() +
  geom_density(data=df, aes(x=value, fill=group), alpha=0.4, color=NA) +
  scale_fill_manual(values=c("#a0a0a0", "#000000")) +
  xlab("value") +
  xlim(x_min, x_max) +
  theme(legend.position="none")


# fit and rgb
# compare fit1, rgb or hsv
df <- data.frame(value=object@extract$mu_r)

rgb <- c(255,255,0)
r <- rgb[1]
g <- rgb[2]
b <- rgb[3]

# limits
x_min <- min(df$value, r)
x_max <- max(df$value, r)
diff <- x_max - x_min
x_min <- x_min - 0.1*diff
x_max <- x_max + 0.1*diff

graph <- ggplot() +
  geom_density(data=df, aes(x=value), alpha=0.4, fill="#000000", color=NA) +
  xlab("value") +
  xlim(x_min, x_max) +
  theme(legend.position="none")

y_max <- ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range

graph <- graph +
  geom_segment(aes(x=r, xend=r, y=0, yend=y_max[2]*1.05), size=1.5, color=rgb(r, g, b, max=255), alpha=0.4) +
  geom_text(aes(label=sprintf("%.2f", r), x=r, y=y_max[2]*1.08), size=4)

## TODO HSV


## plot fit --------------------------------------------------------------




## compare distributions -------------------------------------------------








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
