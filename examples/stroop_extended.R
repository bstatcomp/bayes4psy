library(bayes4psy)
library(plyr)
library(dplyr)
library(ggplot2)

# load and prep data -----------------------------------------------------
df <- read.table("../examples/data/stroop_extended.csv", sep="\t", header=TRUE)

# only correct trials
df <- df %>% filter(acc==1)

age_groups <- c("0-14", "15-29", "30-44", "45-59", "60-74", "75-89")
df$age_group <- cut(df$age,
                    breaks=seq(0,90,15),
                    labels=age_groups)

# treat age groups as subjects in the hierarchical model
df$age_group_id <- mapvalues(df$age_group, age_groups, seq(length(age_groups)))

# test fit ---------------------------------------------------------------
df_test <- df %>% filter(age_group == "75-89", cond == "neutral")

# map subjects to 1..n
subjects <- unique(df_test$subject)
df_test$subject_id <- mapvalues(df_test$subject, subjects, seq(length(subjects)))

# convert factors to int, ignore levels
df_test$subject_id <- as.numeric(as.character(df_test$subject_id))

# fit
fit_test <- b_reaction_time(t=df_test$rt,
                            s=df_test$subject_id,
                            warmup=200,
                            iter=1000,
                            chains=1,
                            control=list(adapt_delta=0.95, max_treedepth=11))

# examine
summary(fit_test)
print(fit_test)
plot_trace(fit_test)
plot_fit(fit_test)
df_subject <- get_parameters(fit_test)

# analysis for neutral condition -----------------------------------------
df_neutral <- df %>% filter(cond == "neutral")

# fit all age groups
fit_list <- list()
for (a in age_groups) {
  df_age <- df_neutral %>% filter(age_group == a)

  # map subjects to 1..n
  subjects <- unique(df_age$subject)
  df_age$subject_id <- mapvalues(df_age$subject, subjects, seq(length(subjects)))

  # convert factors to int, ignore levels
  df_age$subject_id <- as.numeric(as.character(df_age$subject_id))

  # fit
  fit <- b_reaction_time(t=df_age$rt,
                         s=df_age$subject_id,
                         warmup=1000,
                         iter=2000,
                         chains=1,
                         control=list(adapt_delta=0.95, max_treedepth=11))

  fit_list <- append(fit_list, fit)
}

# plot rt with age
df_summary <- data.frame(rt=numeric(),
                         mu=numeric(),
                         sigma=numeric(),
                         lambda=numeric(),
                         age_group=factor())

means <- list()
i <- 1
for (f in fit_list) {
  means[i] <- mean(f@extract$rt)

  df_fit <- get_parameters(f)
  df_fit$age_group <- age_groups[[i]]
  i <- i + 1

  df_summary <- rbind(df_summary, df_fit)
}

levels(df_summary$age_group) <- age_groups

rt_plot <- ggplot(data=df_summary, aes(x=age_group, y=rt, group=age_group)) +
  geom_boxplot() +
  labs(y="reaction time [ms]", x="") +
  ylim(0, 2500)

# which group is the fastest?
df_means <- df_summary %>%
  group_by(age_group) %>%
  summarize(mean_mu=mean(mu),
            mean_sigma=mean(sigma),
            mean_lambda=mean(lambda))

n <- 100000
n_groups <- length(age_groups)
fastest <- rep(0, times=n_groups)
rt <- vector()

for (i in 1:n) {
  for (j in 1:nrow(df_means)) {
    rt[j] <- emg::remg(n=1,
                       mu=df_means[j,]$mean_mu,
                       sigma=df_means[j,]$mean_sigma,
                       lambda=df_means[j,]$mean_lambda)
  }
  fastest_group <- which(rt == min(rt))
  fastest[fastest_group] <- fastest[fastest_group] + 1
}

fastest <- fastest / n
df_fastest <- data.frame(age_group=age_groups, p=fastest)

p_plot <- ggplot(data=df_fastest, aes(x=age_group, y=p)) +
  geom_bar(stat="identity") +
  labs(y="probability", x="age") +
  ylim(0, 1)

# combine both plots
cowplot::plot_grid(rt_plot, p_plot, nrow=2, ncol=1, scale=0.9)
