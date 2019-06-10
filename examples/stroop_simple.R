# libs
library(bayes4psy)
library(ggplot2)

## load data -------------------------------------------------------------
df <- read.table("../examples/data/stroop_simple.csv", sep="\t", header=TRUE)

## ttest fits ------------------------------------------------------------
fit_reading_neutral <- b_ttest(df$reading_neutral)
fit_reading_incongruent <- b_ttest(df$reading_incongruent)
fit_naming_neutral <- b_ttest(df$naming_neutral)
fit_naming_incongruent <- b_ttest(df$naming_incongruent)

# to control the amount of warmup and interation steps use
# fit_reading_neutral <- b_ttest(df$reading_neutral, warmup=5000, iter=6000)

# inspect fits
# print summary
summary(fit_reading_neutral)

# print a more detailed summary (prints the fit object)
# same as show(ttest_results)
print(fit_reading_neutral)

# plot trace
plot_trace(fit_reading_neutral)

# means for single fit
plot_means(fit_reading_neutral)

# visual fit inspection for all 4
# visualize fit quality
plot_fit(fit_reading_neutral)
plot_fit(fit_reading_incongruent)
plot_fit(fit_naming_neutral)
plot_fit(fit_naming_incongruent)


## neutral vs incongruent ------------------------------------------------
# comparisons (compare_means function works like the usual t-test it compares through fitted means)
compare_means(fit_reading_neutral, fit2=fit_reading_incongruent)
compare_means(fit_naming_neutral, fit2=fit_naming_incongruent)

# you can also provide a rope interval
compare_means(fit_reading_neutral, fit2=fit_reading_incongruent, rope=2)

# compare means with a constant value
compare_means(fit_reading_neutral, mu=40)

# or compare means with a normal distribution (used here in Cohen's d calculation)
compare_means(fit_reading_neutral, mu=40, sigma=5)

# plot comparison results
diff_r <- plot_means_difference(fit_reading_neutral, fit2=fit_reading_incongruent)
diff_r <- diff_r + labs(title="Reading")
diff_n <- plot_means_difference(fit_naming_neutral, fit2=fit_naming_incongruent)
diff_n <- diff_n + labs(title="Naming", y="")
cowplot::plot_grid(diff_r, diff_n, nrow=1, ncol=2, scale=0.9)

# you can also provide the rope and bins parameters and compare with a constant value
plot_means_difference(fit_reading_neutral, mu=40, rope=2, bins=10)

# plot fitted means
s_r <- plot_means(fit_reading_neutral, fit2=fit_reading_incongruent)
s_r <- s_r + labs(title="Reading")
s_n <- plot_means(fit_naming_neutral, fit2=fit_naming_incongruent)
s_n <- s_n + labs(title="Naming", y="")
cowplot::plot_grid(s_r, s_n, nrow=1, ncol=2, scale=0.9)

# you can also plot a constant value
plot_means(fit_reading_neutral, mu=40)

# compare distributions (draw samples from fitted distributions and compare)
compare_distributions(fit_reading_neutral, fit2=fit_reading_incongruent)
compare_distributions(fit_naming_neutral, fit2=fit_naming_incongruent)

# plot distribution difference
diff_dr <- plot_distributions_difference(fit_reading_neutral, fit2=fit_reading_incongruent)
diff_dr <- diff_dr + labs(title="Reading")
diff_dn <- plot_distributions_difference(fit_naming_neutral, fit2=fit_naming_incongruent)
diff_dn <- diff_dn + labs(title="Naming", y="")
cowplot::plot_grid(diff_dr, diff_dn, nrow=1, ncol=2, scale=0.9)

# visual comparisons
dist_r <- plot_distributions(fit_reading_neutral, fit2=fit_reading_incongruent)
dist_r <- dist_r + labs(title="Reading")
dist_n <- plot_distributions(fit_naming_neutral, fit2=fit_naming_incongruent)
dist_n <- dist_n + labs(title="Naming", y="")
cowplot::plot_grid(dist_r, dist_n, nrow=1, ncol=2, scale=0.9)


## reading vs naming -----------------------------------------------------
# comparisons
compare_means(fit_reading_neutral, fit2=fit_naming_neutral)
compare_means(fit_reading_incongruent, fit2=fit_naming_incongruent)

# plot comparison results
diff_ne <- plot_means_difference(fit_reading_neutral, fit2=fit_naming_neutral)
diff_ne <- diff_ne + labs(title="Neutral")
diff_i <- plot_means_difference(fit_reading_incongruent, fit2=fit_naming_incongruent)
diff_i <- diff_i + labs(title="Incongruent", y="")
cowplot::plot_grid(diff_ne, diff_i, nrow=1, ncol=2, scale=0.9)

# plot fitted means
s_ne <- plot_means(fit_reading_neutral, fit2=fit_naming_neutral)
s_ne <- s_ne + labs(title="Neutral")
s_i <- plot_means(fit_reading_incongruent, fit2=fit_naming_incongruent)
s_i <- s_i + labs(title="Incongruent", y="")
cowplot::plot_grid(s_ne, s_i, nrow=1, ncol=2, scale=0.9)

# compare distributions (draw samples from fitted distributions and compare)
compare_distributions(fit_reading_neutral, fit2=fit_naming_neutral)
compare_distributions(fit_reading_incongruent, fit2=fit_naming_incongruent)

# plot distribution difference
diff_ne <- plot_distributions_difference(fit_reading_neutral, fit2=fit_naming_neutral)
diff_ne <- diff_ne + labs(title="Neutral")
diff_i <- plot_distributions_difference(fit_reading_incongruent, fit2=fit_naming_incongruent)
diff_i <- diff_i + labs(title="Incongruent", y="")
cowplot::plot_grid(diff_ne, diff_i, nrow=1, ncol=2, scale=0.9)

# visual comparisons
dist_ne <- plot_distributions(fit_reading_neutral, fit2=fit_naming_neutral)
dist_ne <- dist_ne + labs(title="Neutral")
dist_i <- plot_distributions(fit_reading_incongruent, fit2=fit_naming_incongruent)
dist_i <- dist_i + labs(title="Incongruent", y="")
cowplot::plot_grid(dist_ne, dist_i, nrow=1, ncol=2, scale=0.9)
