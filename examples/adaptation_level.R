# libs
library(EasyBayes)
library(dplyr)
library(rstan)

## data wrangling --------------------------------------------------------
# load data
df <- read.table("./examples/data/after_images.csv", sep = "\t")
