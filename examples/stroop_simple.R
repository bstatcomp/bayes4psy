# libs
library(bayes4psy)
library(ggplot2)

## load data ------------------------------------------------------------------
data <- read.table("../examples/data/stroop_simple.csv", sep="\t", header=TRUE)

## ttest fits -----------------------------------------------------------------
# priors
mu_prior <- b_prior(family="normal", pars=c(100, 50))
sigma_prior <- b_prior(family="uniform", pars=c(0, 200))

# attach priors to relevant parameters
priors <- list(c("mu", mu_prior),
               c("sigma", sigma_prior))


# fir
fit_reading_neutral <- b_ttest(data$reading_neutral,
                               priors=priors, iter=3000, warmup=500)
fit_reading_incongruent <- b_ttest(data$reading_incongruent,
                                   priors=priors, iter=3000, warmup=500)
fit_naming_neutral <- b_ttest(data$naming_neutral,
                              priors=priors, iter=3000, warmup=500)
fit_naming_incongruent <- b_ttest(data$naming_incongruent,
                                  priors=priors, iter=3000, warmup=500)

# plot trace
plot_trace(fit_reading_neutral)
plot_trace(fit_reading_incongruent)
plot_trace(fit_naming_neutral)
plot_trace(fit_naming_incongruent)

# check fit (Rhat and n_eff)
print(fit_reading_neutral)
print(fit_reading_incongruent)
print(fit_naming_neutral)
print(fit_naming_incongruent)

# check fits
plot_fit(fit_reading_neutral)
plot_fit(fit_reading_incongruent)
plot_fit(fit_naming_neutral)
plot_fit(fit_naming_incongruent)


## analysis -------------------------------------------------------------------
# 1 second rope interval
rope=1

## cross compare all fits -----------------------------------------------------
fit_list <- c(fit_reading_incongruent, fit_naming_neutral, fit_naming_incongruent)
multiple_comparison <- compare_means(fit_reading_neutral, fits=fit_list, rope=rope)

# plot means
plot_means(fit_reading_neutral, fits=fit_list, rope=rope) +
  scale_fill_hue(labels=c("Reading neutral",
                          "Reading incongruent",
                          "Naming neutral",
                          "Naming incongruent")) +
  theme(legend.title=element_blank())

# plot pairwise difference between means
plot_means_difference(fit_reading_neutral, fits=fit_list, rope=rope)


## neutral vs incongruent -----------------------------------------------------
# comparisons (compare_means function works like the usual t-test it compares through fitted means)
reading_neutral_congruent <- compare_means(fit_reading_neutral, fit2=fit_reading_incongruent, rope=rope)
naming_neutral_congruent <- compare_means(fit_naming_neutral, fit2=fit_naming_incongruent, rope=rope)

# plot comparison results
diff_r <- plot_means_difference(fit_reading_neutral, fit2=fit_reading_incongruent, rope=rope)
diff_r <- diff_r + labs(title="Reading") + theme(legend.position="none")
diff_n <- plot_means_difference(fit_naming_neutral, fit2=fit_naming_incongruent, rope=rope)
diff_n <- diff_n + labs(title="Naming", y="") + theme(legend.position="none")
cowplot::plot_grid(diff_r, diff_n, nrow=1, ncol=2, scale=0.9)

# plot fitted means
means_r <- plot_means(fit_reading_neutral, fit2=fit_reading_incongruent)
means_r <- means_r + labs(title="Reading")
means_n <- plot_means(fit_naming_neutral, fit2=fit_naming_incongruent)
means_n <- means_n + labs(title="Naming", y="")
cowplot::plot_grid(means_r, means_n, nrow=1, ncol=2, scale=0.9)


## reading vs naming -----------------------------------------------------
# comparisons
neutral_reading_naming <- compare_means(fit_reading_neutral, fit2=fit_naming_neutral, rope=rope)
incongruent_reading_naming <- compare_means(fit_reading_incongruent, fit2=fit_naming_incongruent, rope=rope)

# plot comparison results
diff_ne <- plot_means_difference(fit_reading_neutral, fit2=fit_naming_neutral, rope=rope)
diff_ne <- diff_ne + labs(title="Neutral")
diff_i <- plot_means_difference(fit_reading_incongruent, fit2=fit_naming_incongruent, rope=rope)
diff_i <- diff_i + labs(title="Incongruent", y="")
cowplot::plot_grid(diff_ne, diff_i, nrow=1, ncol=2, scale=0.9)

# plot fitted means
means_ne <- plot_means(fit_reading_neutral, fit2=fit_naming_neutral)
means_ne <- means_ne + labs(title="Neutral") + theme(legend.position="none")
means_i <- plot_means(fit_reading_incongruent, fit2=fit_naming_incongruent)
means_i <- means_i + labs(title="Incongruent", y="") + theme(legend.position="none")
cowplot::plot_grid(means_ne, means_i, nrow=1, ncol=2, scale=0.9)
