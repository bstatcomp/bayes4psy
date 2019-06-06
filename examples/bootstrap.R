# libs
library(bayes4psy)
library(ggplot2)
library(dplyr)

## data wrangling --------------------------------------------------------
# load data
df <- read.table("../examples/data/adaptation_level.csv", sep="\t", header=TRUE)

## group 1 data only
df <- df[df$group == 1, ]

# samples
df_b <- df[df$part == 1, ]

# after
df_a <- df[df$part == 2, ]

# linear function of seqence vs. response
lm_statistic <- function(data) {
  lm(sequence ~ response, data)$coef
}

# bootstrap
boot_b <- b_bootstrap(df_b, lm_statistic, n1=1000, n2=1000)
boot_a <- b_bootstrap(df_a, lm_statistic, n1=1000, n2=1000)

# prepare for plotting
boot_b$type <- "before"
boot_a$type <- "after"

# beautify
colnames(boot_b) <- c("intercept", "slope", "type")
colnames(boot_a) <- c("intercept", "slope", "type")

# take n random rows from both
n <- 100
df_random_a <- sample_n(boot_a, n)
df_random_b <- sample_n(boot_b, n)
df_random <- rbind(df_random_a, df_random_b)

# get mean
df_mean <- data.frame(intercept=mean(boot_b$intercept), slope=mean(boot_b$slope), type="before")
df_mean <- rbind(df_mean, data.frame(intercept=mean(boot_a$intercept), slope=mean(boot_a$slope), type="after"))

# plot
ggplot() +
  geom_abline(data=df_random, aes(slope=slope, intercept=intercept, color=type), alpha=0.1, size=1) +
  geom_abline(data=df_mean, aes(slope=slope, intercept=intercept, color=type), size=1.5) +
  xlim(0, 10) +
  ylim(0, 10) +
  scale_color_manual(values=c("#3182bd", "#ff4e3f")) +
  theme(legend.title=element_blank())
