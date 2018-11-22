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

# summary
summary(al_1b)

# check fits
plot_fit(al_1b)

# traceplot
traceplot(al_1b)


# after - 2nd part
df_1a <- df_1[df_1$part == 2, ]

# prep data and fit
al_1a <- b_linear(n = nrow(df_1a),
                  m = m,
                  x = df_1a$sequence,
                  y = df_1a$response,
                  s = df_1a$subject)

# summary
summary(al_1a)

# check fits
plot_fit(al_1a)

# traceplot
traceplot(al_1a)
