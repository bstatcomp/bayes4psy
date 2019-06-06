# libs
library(bayes4psy)
library(ggplot2)

## data wrangling --------------------------------------------------------
# load data
df <- read.table("../examples/data/adaptation_level.csv", sep="\t", header=TRUE)


## group 1 ---------------------------------------------------------------
df_1 <- df[df$group == 1, ]

# number of subjects
m <- length(unique(df_1$subject))

# map subject to 1..m interval
df_1$subject <- plyr::mapvalues(df_1$subject, from=unique(df_1$subject), to=1:m)

# before - 1st part
df_1b <- df_1[df_1$part == 1, ]

# prep data and fit
al_1b <- b_linear(x = df_1b$sequence,
                  y = df_1b$response,
                  s = df_1b$subject)

# to control the amount of warmup and interation steps use
# b_linear(..., warmup=5000, iter=6000)

# diagnostics
# summary
summary(al_1b)

# print a more detailed summary (prints the fit object)
# same as show(al_1b)
print(al_1b)

# check fits
plot_fit(al_1b)

# plot trace
plot_trace(al_1b)

# plot samples
plot_samples(al_1b)

# after - 2nd part
df_1a <- df_1[df_1$part == 2, ]

# prep data and fit
al_1a <- b_linear(x = df_1a$sequence,
                  y = df_1a$response,
                  s = df_1a$subject)


## group 2 ---------------------------------------------------------------
df_2 <- df[df$group == 2, ]

# number of subjects
m <- length(unique(df_2$subject))

# map subject to 1..m interval
df_2$subject <- plyr::mapvalues(df_2$subject, from=unique(df_2$subject), to=1:m)

# before - 1st part
df_2b <- df_2[df_2$part == 1, ]

# prep data and fit
al_2b <- b_linear(x = df_2b$sequence,
                  y = df_2b$response,
                  s = df_2b$subject)

# after - 2nd part
df_2a <- df_2[df_2$part == 2, ]

# prep data and fit
al_2a <- b_linear(x = df_2a$sequence,
                  y = df_2a$response,
                  s = df_2a$subject)


## compare ---------------------------------------------------------------
# compare_samples (optional rope parameter)
compare_samples(al_1b, fit2=al_1a)

# difference (optional rope parameter)
plot_samples_difference(al_1b, fit2=al_1a)

# visually compare samples
plot_samples(al_1b, fit2=al_1a)

# compare distributions (optional rope parameter)
compare_distributions(al_1b, fit2=al_1a)

# plot_distribution also add labels and same limits for both graphs
graph_b <- plot_distributions(al_1b, al_2b) +
  xlim(0, 10) +
  ylim(0, 10) +
  labs(title="Before", x="measurement", y="weight")

graph_a <- plot_distributions(al_1a, al_2a) +
  xlim(0, 10) +
  ylim(0, 10) +
  labs(title="After", x="measurement", y="")

cowplot::plot_grid(graph_b, graph_a, ncol=2, nrow=1, scale=0.9)

# plot difference between distributions
plot_distributions_difference(al_1b, fit2=al_1a)
