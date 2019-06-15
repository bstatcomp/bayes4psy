library(bayes4psy)
library(plyr)
library(dplyr)
library(ggplot2)

# load and prep data ----------------------------------------------------------
data <- read.table("../examples/data/stroop_extended.csv", sep="\t", header=TRUE)

# only correct trials
data <- data %>% filter(acc==1)

# add age groups
age_groups <- c("0-14", "15-29", "30-44", "45-59", "60-74", "75-89")
data$age_group <- cut(data$age,
                    breaks=seq(0,90,15),
                    labels=age_groups)


# analysis for neutral condition ----------------------------------------------
neutral <- data %>% filter(cond == "neutral")

# fit all age groups
fit_list <- list()
for (a in age_groups) {
  age_group_data <- neutral %>% filter(age_group == a)

  # map subjects to 1..n
  subjects <- unique(age_group_data$subject)
  age_group_data$subject_id <- mapvalues(age_group_data$subject, subjects, seq(length(subjects)))

  # convert factors to int, ignore levels
  age_group_data$subject_id <- as.numeric(as.character(age_group_data$subject_id))

  # fit
  fit <- b_reaction_time(t=age_group_data$rt,
                         s=age_group_data$subject_id,
                         warmup=5000,
                         iter=6000,
                         chains=1)

  fit_list <- append(fit_list, fit)
}

# check if fits are ok, change index to validate for other age groups
print(fit_list[[1]])
plot_trace(fit_list[[1]])

## analysis and plots ---------------------------------------------------------

# plot reaction time with age
# create a data frame with all info
summary <- data.frame(rt=numeric(),
                         mu=numeric(),
                         sigma=numeric(),
                         lambda=numeric(),
                         age_group=factor())

# add data to data frame
i <- 1
for (f in fit_list) {
  fit <- get_parameters(f)
  fit$age_group <- age_groups[[i]]
  i <- i + 1

  summary <- rbind(summary, fit)
}

levels(summary$age_group) <- age_groups

rt_plot <- ggplot(data=summary, aes(x=age_group, y=rt, group=age_group)) +
  geom_boxplot() +
  labs(y="reaction time [ms]", x="") +
  ylim(0, 2500) +
  ggtitle("Reaction times")

# plot probabilities for which group is the fastest/slowest
fit1 <- fit_list[[1]]
other_fits <- fit_list[2:length(fit_list)]
comparison <- compare_distributions(fit1, fits=other_fits)

# extract data about fastest/slowest and create a data frame
fastest <- data.frame(age_group=age_groups,
                         probability=comparison$smallest_largest$smallest)

slowest <- data.frame(age_group=age_groups,
                      probability=comparison$smallest_largest$largest)

# create plots
fastest_plot <- ggplot(data=fastest, aes(x=age_group, y=probability)) +
  geom_bar(stat="identity") +
  labs(y="probability", x="age") +
  ylim(0, 1) +
  ggtitle("Fastest probability")

slowest_plot <- ggplot(data=slowest, aes(x=age_group, y=probability)) +
  geom_bar(stat="identity") +
  labs(y="probability", x="age") +
  ylim(0, 1) +
  ggtitle("Slowest probability")

# combine both plots
cowplot::plot_grid(rt_plot, fastest_plot, slowest_plot, nrow=3, ncol=1, scale=0.9)
