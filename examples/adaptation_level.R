# libs
library(EasyBayes)
library(plyr)
library(dplyr)
library(rstan)
library(ggplot2)


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
stan_data <- list(n = nrow(df_1b),
                  m = m,
                  x = df_1b$sequence,
                  y = df_1b$response,
                  s = df_1b$subject)

model <- stan_model(file = './examples/linear.stan')

fit_1b <- sampling(model,
                data = stan_data,
                warmup = 200,
                iter = 400,
                chains = 1)

#plot fit ----------------------------------------------------------------
df_data <- data.frame(sequence = data$sequence, response = data$response, subject = data$subject)

n <- length(unique(data$subject))

x_min <- floor(min(data$sequence))
x_max <- ceiling(max(data$sequence))

# ncol
n_col <- ceiling(sqrt(n))

# mean per subject
df_data <- df_data %>% group_by(subject, sequence) %>% summarize(response = mean(response, na.rm=TRUE))

# density per subject
#graph <-

ggplot(df_data, aes(x = sequence, y = response)) +
  geom_point(color = "#3182bd", alpha = 0.4) +
  facet_wrap(~ subject, ncol = n_col) +
  theme_minimal()


# before - 1st part
df_1a <- df_1[df_1$part == 2, ]

# prep data and fit
stan_data <- list(n = nrow(df_1a),
                  m = m,
                  x = df_1a$sequence,
                  y = df_1a$response,
                  s = df_1a$subject)

fit_1a <- sampling(model,
                   data = stan_data,
                   warmup = 200,
                   iter = 400,
                   chains = 1)


