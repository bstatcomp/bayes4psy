library(EasyBayes)
library(plyr)
library(dplyr)
library(ggplot2)

# load data
df <- read.table("../examples/data/stroop_extended.csv", sep="\t", header=TRUE)

# only correct trials
df <- df %>% filter(acc==1)

age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
df$age_group <- cut(df$age,
                    breaks=seq(0,90,10),
                    labels=age_groups)

# treat age groups as subjects in the hierarchical model
df$age_group_id <- mapvalues(df$age_group, age_groups, seq(length(age_groups)))

# test fit of one age group for analysis
df_test <- df_neutral %>% filter(age_group == "30-39")
# map subjects to 1..n
subjects <- unique(df_test$subject)
df_test$subject_id <- mapvalues(df_test$subject, subjects, seq(length(subjects)))

# convert factors to int, ignore levels
df_test$subject_id <- as.numeric(as.character(df_test$subject_id))

fit_test <- b_reaction_time(t=df_test$rt, s=df_test$subject_id, warmup=2000, iter=3000, chains=1)

# examine
summary(fit_test)
print(fit_test)
plot_trace(fit_test)
plot_fit(fit_test)

object <- fit_test
df_subject <- get_subject_samples(fit_test)

# fit
df_neutral <- df %>% filter(cond == "neutral")
fit_neutral <- b_reaction_time(t=df_neutral$rt, s=df_test$subject_id, warmup=2000, iter=3000, chains=1)

fit_list <- list()
# fit all age groups
for (a in age_groups) {
  df_age <- df_neutral %>% filter(age_group == a)

  # map subjects to 1..n
  subjects <- unique(df_age$subject)
  df_age$subject_id <- mapvalues(df_age$subject, subjects, seq(length(subjects)))

  # convert factors to int, ignore levels
  df_age$subject_id <- as.numeric(as.character(df_age$subject_id))

  # fit
  fit <- b_reaction_time(t=df_age$rt, s=df_age$subject_id, warmup=500, iter=1500, chains=1)
  fit_list <- append(fit_list, fit)
}

# plot rt with age
i <- 1
df_summary <- data.frame(mu=numeric(), sigma=numeric(), lambda=numeric(), age_group=factor())
means <- list()
for (f in fit_list) {
  means[i] <- mean(f@extract$mu)

  df_fit <- get_samples(f)
  df_fit$age_group <- age_groups[[i]]
  i <- i + 1

  df_summary <- rbind(df_summary, df_fit)
}

levels(df_summary$age_group) <- age_groups

df_summary$rt <- df_summary$mu + 1/df_summary$lambda

df_means <- df_summary %>% group_by(age_group) %>%
  summarize(mean_rt=mean(rt), low=mean_rt-(1.96*mean(sigma)), high=mean_rt+(1.96*mean(sigma)))

ggplot(data=df_summary, aes(x=age_group, y=rt, group=1)) +
  geom_smooth(method="loess") +
  ylim(0, 2000)

ggplot(data=df_means, aes(x=age_group, y=mean_rt, group=1)) +
  geom_ribbon(aes(ymin=low, ymax=high), fill="#000000", alpha=0.2) +
  geom_line(color="#000000")

ggplot(data=df_summary, aes(x=age_group, y=rt, group=age_group)) +
  geom_boxplot()


# which group is the fastest?
n <- 100000
n_groups <- length(age_groups)
fastest <- rep(0, times=n_groups)
rt <- vector()

for (i in 1:n) {
  for (params in df_means) {
    rt[j] <- remg(mu=params$meanmu_m1, sigma=mu_s1, lambda=mu_l1)
  }

}

fastest <- fastest / n
df_fastest <- data.frame(age_group=age_groups, p=fastest)

