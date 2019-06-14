# libs
library(bayes4psy)
library(ggplot2)
library(dplyr)

## data wrangling --------------------------------------------------------
# load data
data <- read.table("../examples/data/adaptation_level.csv", sep="\t", header=TRUE)

## group 1 data only
data <- data[data$group == 1, ]

# samples
part1 <- data[data$part == 1, ]

# after
part2 <- data[data$part == 2, ]

# linear function of seqence vs. response
lm_statistic <- function(data) {
  lm(sequence ~ response, data)$coef
}

# bootstrap
boot_part1 <- b_bootstrap(part1, lm_statistic, n1=1000, n2=1000)
boot_part2 <- b_bootstrap(part2, lm_statistic, n1=1000, n2=1000)

# prepare for plotting
boot_part1$type <- "First part"
boot_part2$type <- "Second part"

# beautify
colnames(boot_part1) <- c("intercept", "slope", "type")
colnames(boot_part2) <- c("intercept", "slope", "type")

# take n random rows from both
n <- 100
random_part1 <- sample_n(boot_part1, n)
random_part2 <- sample_n(boot_part2, n)
random <- rbind(random_part1, random_part2)

# get mean
means <- data.frame(intercept=mean(boot_part1$intercept), slope=mean(boot_part1$slope), type="First part")
means <- rbind(means, data.frame(intercept=mean(boot_part2$intercept), slope=mean(boot_part2$slope), type="Second part"))

# plot
ggplot() +
  geom_abline(data=random, aes(slope=slope, intercept=intercept, color=type), alpha=0.1, size=1) +
  geom_abline(data=means, aes(slope=slope, intercept=intercept, color=type), size=1.5) +
  xlim(0, 10) +
  ylim(0, 10) +
  scale_color_manual(values=c("#3182bd", "#ff4e3f")) +
  theme(legend.title=element_blank())
