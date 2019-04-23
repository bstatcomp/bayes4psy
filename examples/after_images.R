# libs
library(EasyBayes)
library(dplyr)


## data wrangling --------------------------------------------------------
# load data
df_all <- read.table("../examples/data/after_images.csv", sep="\t", header=TRUE)

## BLUE
df_blue <- df_all %>% filter(stimuli == "blue")
n <- nrow(df_blue) # number of measurements
df <- data.frame(r = df_blue$r, g=df_blue$g, b=df_blue$b)
fit_blue <- b_color(df)

# print summary
summary(fit_blue)

# print a more detailed summary (prints the fit object)
# same as show(ttest_results)
print(fit_blue)

# visualize fit quality
plot_fit(fit_blue)

# visualize fit quality through hsv plot
plot_hsvfit(fit_blue)

# plot trace
plot_trace(fit_blue)

# samples for single fit
plot_samples(fit_blue)

# distribution for single fit
plot_distributions(fit_blue)


## RED
df_red <- df_all %>% filter(stimuli == "red")
n <- nrow(df_red) # number of measurements
df <- data.frame(r = df_red$r, g=df_red$g, b=df_red$b)
fit_red <- b_color(df)

# visualize fit quality
plot_fit(fit_red, par=c("h", "s", "v"))

# visualize fit quality through hsv plot
plot_hsvfit(fit_red)

# samples for single fit
plot_samples(fit_red)

# distribution for single fit
plot_distributions(fit_red)

# compare
compare(fit_blue, fit_red)

# plot_difference
plot_difference(fit_blue, fit_red)

# plot_samples
plot_samples(fit_blue, fit_red)

# compare_distributions
compare_distributions(fit_blue, fit_red)

# plot_distributions
plot_distributions(fit_blue, fit_red)

# plot_distributions_difference
plot_distributions_difference(fit_blue, fit_red)


# add stimuli data
colors <- c("cyan", "magenta", "blue", "yellow", "green", "red")
df_stimuli <- expand.grid(r_s = c(255, 0), g_s = c(255, 0), b_s = c(255, 0))[c(-1, -8), ] %>%
  mutate(stimuli = factor(colors), levels = levels(colors)) %>%
  arrange(stimuli)
df_stimuli[c("h_s", "s_s", "v_s")] <- with(df_stimuli, t(rgb2hsv(r_s, g_s, b_s, maxColorValue = 255)))
# merge stimuli data with measurements
df2 <- inner_join(df, df_stimuli)

