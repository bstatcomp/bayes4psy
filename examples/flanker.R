# libs
library(EasyBayes)

# load data --------------------------------------------------------------------
df <- read.table("./examples/data/flanker.csv", sep = "\t")

# map correct/incorrect/timeout to 1/0
df$result_numeric <- 0
df[df$result == "correct", ]$result_numeric <- 1

# test vs control rection times (correct and no timeout only)
df_correct <- df[df$result == "correct", ]

# countrol group
df_control <- df_correct[df_correct$group == "control", ]

# subject indexes range on 22..45 cast to 1..23
df_control$subject <- df_control$subject - 21

n <- nrow(df_control)
m <- length(unique(df_control$subject))
rt <- df_control$rt
r <- df_control$result_numeric
s <- df_control$subject

rt_control <- b_reaction_times(n = n, m = m, rt = rt, r = r, s = s)

#summary
summary(rt_control)

# check fits
plot_fit(rt_control)

# test group
df_test <- df_correct[df_correct$group == "test", ]

n <- nrow(df_test)
m <- length(unique(df_test$subject))
rt <- df_test$rt
r <- df_test$result_numeric
s <- df_test$subject

rt_test <- b_reaction_times(n = n, m = m, rt = rt, r = r, s = s)

#summary
summary(rt_test)

# check fits
plot_fit(rt_test)

# difference summary


# difference plot
temp <- plot_difference(rt_control)
temp <- plot_difference(rt_control, df)
plot_difference(rt_control, rt_test)





object1 <- rt_control
object2 <- rt_test

# compare two fits
x <- seq(0, 2, 0.01)
df1 <- data.frame(x = x, y = demg(x,
                                  mu = mean(object1@extract$mu_m),
                                  sigma = mean(object1@extract$mu_s),
                                  lambda = mean(object1@extract$mu_l)))

df2 <- data.frame(x = x, y = demg(x,
                                  mu = mean(object2@extract$mu_m),
                                  sigma = mean(object2@extract$mu_s),
                                  lambda = mean(object2@extract$mu_l)))

graph <- ggplot() +
  geom_area(data = df1, aes(x = x, y = y), fill = "#3182bd", alpha = 0.3, color = NA) +
  geom_area(data = df2, aes(x = x, y = y), fill = "#ff4e3f", alpha = 0.3, color = NA) +
  theme_minimal() +
  xlab("Reaction time")

graph


# TODO:
# Hypothesis: group A mean < group B mean
diff <- (object1@extract$mu_m + 1/object1@extract$mu_l) - (object2@extract$mu_m + 1/object2@extract$mu_l)

# 1 > 2
group1_smaller <- sum(diff > 0) / length(diff);
cat(sprintf("Probabilities for reaction time comaprison:\n  - Group 1 < Group 2: %.2f", group1_smaller))

# 2 > 1
group1_greater <- sum(diff < 0) / length(diff);
cat(sprintf("\n  - Group 1 > Group 2: %.2f", group1_greater))

diff_l <- quantile(diff12, 0.025)
diff_h <- quantile(diff12, 0.975)
cat(sprintf("\n95%% CI for reaction times:\n  - Group 1 - Group 2: [%.2f, %.2f]", diff_l, diff_h))

p <- sum(diff > 0) / length(diff);
diff_l <- quantile(diff, 0.025)
diff_h <- quantile(diff, 0.975)
cat(sprintf("p(Group 1 < Group 2): %.2f\n95%% CI: %.2f - %.2f\n", p, diff_l, diff_h))


e1 <- object1@extract$a / (object1@extract$a + object1@extract$b)
e2 <- object2@extract$a / (object2@extract$a + object2@extract$b)


# TODO: PER SUBJECT COMPARISON


