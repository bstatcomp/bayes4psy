# libs
library(EasyBayes)


## data wrangling --------------------------------------------------------
# load data
df <- read.table("./examples/data/adaptation_level.csv", sep="\t", header=TRUE)


## group 1 ---------------------------------------------------------------
df_1 <- df[df$group == 1, ]

# number of subjects
m = length(unique(df_1$subject))

# map subject to 1..m interval
df_1$subject <- mapvalues(df_1$subject, from = unique(df_1$subject), to=1:m)

# before - 1st part
df_1b <- df_1[df_1$part == 1, ]

# prep data and fit
al_1b <- b_linear(n = nrow(df_1b),
                 m = m,
                 x = df_1b$sequence,
                 y = df_1b$response,
                 s = df_1b$subject)

# diagnostics
# summary
summary(al_1b)

# check fits
plot_fit(al_1b)

# traceplot
traceplot(al_1b)

# plot samples
plot_samples(al_1b)


# after - 2nd part
df_1a <- df_1[df_1$part == 2, ]

# prep data and fit
al_1a <- b_linear(n = nrow(df_1a),
                  m = m,
                  x = df_1a$sequence,
                  y = df_1a$response,
                  s = df_1a$subject)


## group 2 ---------------------------------------------------------------
df_2 <- df[df$group == 2, ]

# number of subjects
m = length(unique(df_2$subject))

# map subject to 1..m interval
df_2$subject <- mapvalues(df_2$subject, from = unique(df_2$subject), to=1:m)

# before - 1st part
df_2b <- df_2[df_2$part == 1, ]

# prep data and fit
al_2b <- b_linear(n = nrow(df_2b),
                  m = m,
                  x = df_2b$sequence,
                  y = df_2b$response,
                  s = df_2b$subject)

# after - 2nd part
df_2a <- df_2[df_2$part == 2, ]

# prep data and fit
al_2a <- b_linear(n = nrow(df_2a),
                  m = m,
                  x = df_2a$sequence,
                  y = df_2a$response,
                  s = df_2a$subject)


## compare ---------------------------------------------------------------
# compare (optional rope parameter)
compare(al_1b, fit2 = al_1a)

# difference (optional rope parameter)
plot_difference(al_1b, fit2 = al_1a)

# visually compare samples
plot_samples(al_1b, fit2 = al_1a)

# compare distributions (optional rope parameter)
compare_distributions(al_1b, fit2 = al_1a)

# plot_distribution
graph_b <- plot_distributions(al_1b, al_2b)
graph_a <- plot_distributions(al_1a, al_2a)
plot_grid(graph_b, graph_a, ncol = 2, nrow = 1, scale = 0.9)

# plot difference between distributions
plot_distributions_difference(al_1b, fit2 = al_1a)
