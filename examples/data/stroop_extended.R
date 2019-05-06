df <- read.csv("stroop_extended.csv", sep="\t")
df$age_group <- cut(df$age,
                    breaks=seq(0,90,10),
                    labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"))
