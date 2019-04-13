# libs
library(EasyBayes)
library(dplyr)
library(rstan)


## data wrangling --------------------------------------------------------
# load data
df_all <- read.table("../examples/data/after_images.csv", sep="\t", header=TRUE)

## BLUE
df_blue <- df_all %>% filter(stimuli == "blue")
n <- nrow(df_blue) # number of measurements
df <- data.frame(r = df_blue$r, g=df_blue$g, b=df_blue$b)
fit1 <- b_color(df)

## RED
df_red <- df_all %>% filter(stimuli == "red")
n <- nrow(df_red) # number of measurements
df <- data.frame(r = df_blue$r, g=df_blue$g, b=df_blue$b)
fit1 <- b_color(df)


# add stimuli data
colors <- c("cyan", "magenta", "blue", "yellow", "green", "red")
df_stimuli <- expand.grid(r_s = c(255, 0), g_s = c(255, 0), b_s = c(255, 0))[c(-1, -8), ] %>%
  mutate(stimuli = factor(colors), levels = levels(colors)) %>%
  arrange(stimuli)
df_stimuli[c("h_s", "s_s", "v_s")] <- with(df_stimuli, t(rgb2hsv(r_s, g_s, b_s, maxColorValue = 255)))
# merge stimuli data with measurements
df2 <- inner_join(df, df_stimuli)

