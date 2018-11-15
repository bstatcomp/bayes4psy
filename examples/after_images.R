# libs
library(EasyBayes)
library(dplyr)
library(rstan)

## data wrangling --------------------------------------------------------
# load data
df <- read.table("./examples/data/after_images.csv", sep = "\t")

# stimuli to indexes
stimuli <- unique(df$stimuli)
indexes <- 1:length(stimuli)
df$stimuli_index <- as.integer(factor(df$stimuli, levels=stimuli, labels=indexes))


## colour analysis--------------------------------------------------------
n <- nrow(df) # number of measurements
m <- length(unique(df$stimuli)) # number of different stimuili
stimuli <- df$stimuli_index
r <- df$r
g <- df$g
b <- df$b

# cast to hsv (do this inside lib late ron)
df[c("h", "s", "v")] <- with(df, t(rgb2hsv(r, g, b, maxColorValue = 255)))
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

model <- stan_model(file = './examples/colours.stan')

fit <- sampling(model,
                  data = stan_data,
                  warmup = 500,
                  iter = 1000,
                  chains = 1)

extract <- rstan::extract(fit)
