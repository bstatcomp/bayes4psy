# libs
library(EasyBayes)
library(dplyr)


## data wrangling and fitting --------------------------------------------
color_levels <- c("red", "green", "blue", "yellow", "cyan", "magenta")

# load data
df_all <- read.table("../examples/data/after_images.csv", sep="\t", header=TRUE)
levels(df_all$stimuli) <- color_levels

# add stimuli data
colors <- c("cyan", "magenta", "blue", "yellow", "green", "red")
df_stimuli <- expand.grid(r_s=c(255, 0), g_s=c(255, 0), b_s=c(255, 0))[c(-1, -8), ] %>%
  mutate(stimuli=factor(colors, levels=color_levels)) %>%
  arrange(stimuli)
df_stimuli[c("h_s", "s_s", "v_s")] <- with(df_stimuli, t(rgb2hsv(r_s, g_s, b_s, maxColorValue=255)))

df_stimuli$h_s <- df_stimuli$h_s * 2*pi
# merge stimuli data with measurements
df_all <- inner_join(df_all, df_stimuli)


## blue ------------------------------------------------------------------
df_blue <- df_all %>% filter(stimuli == "blue")
n <- nrow(df_blue) # number of measurements
df <- data.frame(r=df_blue$r, g=df_blue$g, b=df_blue$b)
fit_blue <- b_color(df)

# print summary
summary(fit_blue)

# print a more detailed summary (prints the fit object)
# same as show(ttest_results)
print(fit_blue)

# visualize fit quality
plot_fit(fit_blue)

# visualize fit quality through hsv plot
plot_fit_hsv(fit_blue)

# plot trace
plot_trace(fit_blue)

# samples for single fit
plot_samples(fit_blue)

# distribution for single fit
plot_distributions(fit_blue)


## red -------------------------------------------------------------------
df_red <- df_all %>% filter(stimuli == "red")
n <- nrow(df_red) # number of measurements
df <- data.frame(r=df_red$r, g=df_red$g, b=df_red$b)
fit_red <- b_color(df)

# visualize fit quality through hsv plot
plot_fit_hsv(fit_red)

# compare
compare(fit_blue, fit_red)

# plot_difference
plot_difference(fit_blue, fit_red)

# plot_difference
plot_difference_hsv(fit_blue, fit_red)

# plot_samples
plot_samples(fit_blue, fit_red)

# compare_distributions
compare_distributions(fit_blue, fit_red)

# plot_distributions
plot_distributions(fit_blue, fit_red)

# plot_distributions_difference
plot_distributions_difference(fit_blue, fit_red)


## green -----------------------------------------------------------------
df_green <- df_all %>% filter(stimuli == "green")
n <- nrow(df_green) # number of measurements
df <- data.frame(r=df_green$r, g=df_green$g, b=df_green$b)
fit_green <- b_color(df)


## yellow ----------------------------------------------------------------
df_yellow <- df_all %>% filter(stimuli == "yellow")
n <- nrow(df_yellow) # number of measurements
df <- data.frame(r=df_yellow$r, g=df_yellow$g, b=df_yellow$b)
fit_yellow <- b_color(df)


## magenta ---------------------------------------------------------------
df_magenta <- df_all %>% filter(stimuli == "magenta")
n <- nrow(df_magenta) # number of measurements
df <- data.frame(r=df_magenta$r, g=df_magenta$g, b=df_magenta$b)
fit_magenta <- b_color(df)


## cyan ------------------------------------------------------------------
df_cyan <- df_all %>% filter(stimuli == "cyan")
n <- nrow(df_cyan) # number of measurements
df <- data.frame(r=df_cyan$r, g=df_cyan$g, b=df_cyan$b)
fit_cyan <- b_color(df)

## analysis plots --------------------------------------------------------
# predicted afterimages of trichromatic theory
df_trichromatic <- data.frame(stimuli = factor(color_levels, levels=color_levels),
                              r = c(0, 255, 255, 0, 255, 0),
                              g = c(255, 0, 255, 0, 0, 255),
                              b = c(255, 255, 0, 255, 0, 0))
df_trichromatic[c("h", "s", "v")] <- with(df_trichromatic, t(rgb2hsv(r, g, b, maxColorValue=255)))
df_trichromatic$h <- df_trichromatic$h * 2*pi

# predicted afterimages of opponent-process theory
df_opponent_process <- data.frame(stimuli = factor(color_levels, levels=color_levels),
                                  r = c(0, 255, 255, 0, 255, 127),
                                  g = c(255, 0, 255, 0, 127, 255),
                                  b = c(0, 0, 0, 255, 0, 0))
df_opponent_process[c("h", "s", "v")] <- with(df_opponent_process, t(rgb2hsv(r, g, b, maxColorValue=255)))
df_opponent_process$h <- df_opponent_process$h * 2*pi

# red
stimuli <- "red"
lines <- list()
lines[[1]] <- c(df_trichromatic[df_trichromatic$stimuli == stimuli, ]$h,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$s,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$v)
lines[[2]] <- c(df_opponent_process[df_opponent_process$stimuli == stimuli, ]$h,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$s,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$v)

points <- list()
points[[1]] <- c(df_stimuli[df_stimuli$stimuli == stimuli, ]$h_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$s_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$v_s)

plot_red <- plot_difference_hsv(fit_red, points=points, lines=lines, hsv=TRUE)
plot_red <- plot_red + ggtitle("Red") + theme(plot.title = element_text(hjust = 0.5))

plot_red_d <- plot_distributions_difference_hsv(fit_red, points=points, lines=lines, hsv=TRUE)
plot_red_d <- plot_red_d + ggtitle("Red") + theme(plot.title = element_text(hjust = 0.5))


# blue
stimuli <- "blue"
lines <- list()
lines[[1]] <- c(df_trichromatic[df_trichromatic$stimuli == stimuli, ]$h,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$s,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$v)
lines[[2]] <- c(df_opponent_process[df_opponent_process$stimuli == stimuli, ]$h,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$s,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$v)

points <- list()
points[[1]] <- c(df_stimuli[df_stimuli$stimuli == stimuli, ]$h_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$s_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$v_s)

plot_blue <- plot_difference_hsv(fit_blue, points=points, lines=lines, hsv=TRUE)
plot_blue <- plot_blue + ggtitle("Blue") + theme(plot.title = element_text(hjust = 0.5))

plot_blue_d <- plot_distributions_difference_hsv(fit_blue, points=points, lines=lines, hsv=TRUE)
plot_blue_d <- plot_blue_d + ggtitle("Blue") + theme(plot.title = element_text(hjust = 0.5))


# green
stimuli <- "green"
lines <- list()
lines[[1]] <- c(df_trichromatic[df_trichromatic$stimuli == stimuli, ]$h,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$s,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$v)
lines[[2]] <- c(df_opponent_process[df_opponent_process$stimuli == stimuli, ]$h,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$s,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$v)

points <- list()
points[[1]] <- c(df_stimuli[df_stimuli$stimuli == stimuli, ]$h_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$s_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$v_s)

plot_green <- plot_difference_hsv(fit_green, points=points, lines=lines, hsv=TRUE)
plot_green <- plot_green + ggtitle("Green") + theme(plot.title = element_text(hjust = 0.5))

plot_green_d <- plot_distributions_difference_hsv(fit_green, points=points, lines=lines, hsv=TRUE)
plot_green_d <- plot_green_d + ggtitle("Green") + theme(plot.title = element_text(hjust = 0.5))


# yellow
stimuli <- "yellow"
lines <- list()
lines[[1]] <- c(df_trichromatic[df_trichromatic$stimuli == stimuli, ]$h,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$s,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$v)
lines[[2]] <- c(df_opponent_process[df_opponent_process$stimuli == stimuli, ]$h,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$s,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$v)

points <- list()
points[[1]] <- c(df_stimuli[df_stimuli$stimuli == stimuli, ]$h_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$s_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$v_s)

plot_yellow <- plot_difference_hsv(fit_yellow, points=points, lines=lines, hsv=TRUE)
plot_yellow <- plot_yellow + ggtitle("Yellow") + theme(plot.title = element_text(hjust = 0.5))

plot_yellow_d <- plot_distributions_difference_hsv(fit_yellow, points=points, lines=lines, hsv=TRUE)
plot_yellow_d <- plot_yellow_d + ggtitle("Yellow") + theme(plot.title = element_text(hjust = 0.5))


# cyan
stimuli <- "cyan"
lines <- list()
lines[[1]] <- c(df_trichromatic[df_trichromatic$stimuli == stimuli, ]$h,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$s,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$v)
lines[[2]] <- c(df_opponent_process[df_opponent_process$stimuli == stimuli, ]$h,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$s,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$v)

points <- list()
points[[1]] <- c(df_stimuli[df_stimuli$stimuli == stimuli, ]$h_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$s_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$v_s)

plot_cyan <- plot_difference_hsv(fit_cyan, points=points, lines=lines, hsv=TRUE)
plot_cyan <- plot_cyan + ggtitle("Cyan") + theme(plot.title = element_text(hjust = 0.5))

plot_cyan_d <- plot_distributions_difference_hsv(fit_cyan, points=points, lines=lines, hsv=TRUE)
plot_cyan_d <- plot_cyan_d + ggtitle("Cyan") + theme(plot.title = element_text(hjust = 0.5))


# cyan
stimuli <- "magenta"
lines <- list()
lines[[1]] <- c(df_trichromatic[df_trichromatic$stimuli == stimuli, ]$h,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$s,
                df_trichromatic[df_trichromatic$stimuli == stimuli, ]$v)
lines[[2]] <- c(df_opponent_process[df_opponent_process$stimuli == stimuli, ]$h,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$s,
                df_opponent_process[df_opponent_process$stimuli == stimuli, ]$v)

points <- list()
points[[1]] <- c(df_stimuli[df_stimuli$stimuli == stimuli, ]$h_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$s_s,
                 df_stimuli[df_stimuli$stimuli == stimuli, ]$v_s)

plot_magenta <- plot_difference_hsv(fit_magenta, points=points, lines=lines, hsv=TRUE)
plot_magenta <- plot_magenta + ggtitle("Magenta") + theme(plot.title = element_text(hjust = 0.5))

plot_magenta_d <- plot_distributions_difference_hsv(fit_magenta, points=points, lines=lines, hsv=TRUE)
plot_magenta_d <- plot_magenta_d + ggtitle("Magenta") + theme(plot.title = element_text(hjust = 0.5))

# plot grid
cowplot::plot_grid(plot_red, plot_green, plot_blue, plot_yellow, plot_cyan, plot_magenta, ncol=3, nrow=2, scale=0.9)
cowplot::plot_grid(plot_red_d, plot_green_d, plot_blue_d, plot_yellow_d, plot_cyan_d, plot_magenta_d, ncol=3, nrow=2, scale=0.9)


# tiles plot
# get averages
df_avg <- data.frame(r=numeric(), g=numeric(), b=numeric, stimuli=factor(), order=numeric())
df_avg <- rbind(df_avg, data.frame(r=mean(fit_red@extract$mu_r),
                                   g=mean(fit_red@extract$mu_g),
                                   b=mean(fit_red@extract$mu_b),
                                   stimuli="red"))
df_avg <- rbind(df_avg, data.frame(r=mean(fit_green@extract$mu_r),
                                   g=mean(fit_green@extract$mu_g),
                                   b=mean(fit_green@extract$mu_b),
                                   stimuli="green"))
df_avg <- rbind(df_avg, data.frame(r=mean(fit_blue@extract$mu_r),
                                   g=mean(fit_blue@extract$mu_g),
                                   b=mean(fit_blue@extract$mu_b),
                                   stimuli="blue"))
df_avg <- rbind(df_avg, data.frame(r=mean(fit_yellow@extract$mu_r),
                                   g=mean(fit_yellow@extract$mu_g),
                                   b=mean(fit_yellow@extract$mu_b),
                                   stimuli="yellow"))
df_avg <- rbind(df_avg, data.frame(r=mean(fit_cyan@extract$mu_r),
                                   g=mean(fit_cyan@extract$mu_g),
                                   b=mean(fit_cyan@extract$mu_b),
                                   stimuli="cyan"))
df_avg <- rbind(df_avg, data.frame(r=mean(fit_magenta@extract$mu_r),
                                   g=mean(fit_magenta@extract$mu_g),
                                   b=mean(fit_magenta@extract$mu_b),
                                   stimuli="magenta"))
levels(df_avg$stimuli) <- color_levels

# add hsv
df_avg[c("h", "s", "v")] <- t(rgb2hsv(df_avg$r, df_avg$g, df_avg$b, maxColorValue = 255))

# add stimuli
df_avg <- inner_join(df_avg, df_stimuli)

# plot
tiles_plot <- ggplot(df_avg, aes(ymin=-as.numeric(stimuli)*10 + 1, ymax=-as.numeric(stimuli)*10 + 10)) +
  geom_rect(aes(fill=rgb(r_s, g_s, b_s, maxColorValue=255), xmin=11, xmax=20)) +
  geom_rect(data=df_trichromatic, aes(fill=rgb(r, g, b, maxColorValue=255)), xmin=31-5, xmax=40-5) +
  geom_rect(data=df_opponent_process, aes(fill=rgb(r, g, b, maxColorValue=255)), xmin=41-5, xmax=50-5) +
  geom_rect(xmin=61-10, xmax=70-10, aes(fill=rgb(r, g, b, maxColorValue=255), xmin=11, xmax=20)) +
  geom_rect(xmin=71-10, xmax=80-10, aes(fill=hsv(h, 1, 1))) +
  scale_fill_identity() +
  coord_equal(ylim=c(-59-10, 0+10), xlim=c(11-10, 70+10)) +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill = "gray80"),
              plot.margin=unit(c(-0.5,-0.5,-0.5,-0.5),"line")) +
  annotate("text", x=15.3, y=2, label="Stimuli", size=5) +
  annotate("text", x=35.5, y=2, label="Prediction", size=5) +
  annotate("text", x=60.5, y=2, label="Response", size=5)

tiles_plot
