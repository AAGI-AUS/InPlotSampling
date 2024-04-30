pacman::p_load(SDaA, FNN, ggplot2, tidyverse)

load("data/POP_Corrugated.RData")
load("data/POP_Peak.RData")
load("data/POP_Bird.RData")

source("R/bootstrap_sample.R")
source("R/get_empirical_population.R")
source("R/HEADMAP_F.R")
source("R/inclusion_probability.R")
source("R/sbs_pps_sample.R")
source("R/SBS_PPSF.R")
source("R/utils.R")

set.seed(112)

# SBS sample size, PPS sample size
sample_sizes <- c(20, 20)

population <- POP_Corrugated
y <- population[, 4]
population <- population[, -4]

sample_result <- sbs_pps_sample(population, sample_sizes)
print(sample_result)

# estimate the population mean and construct a confidence interval
df_sample <- sample_result[[2]]
sample_id <- df_sample[, 1]
y_sample <- y[sample_id]
estimated <- sbs_pps_estimate(
  population, sample_sizes, y_sample,
  sample_matrix = df_sample, n_bootstrap = 100, alpha = 0.05
)
print(estimated)

# pps only
sample_sizes <- c(0, 20)

sample_result <- sbs_pps_sample(population, sample_sizes)
print(sample_result)

# estimate the population mean and construct a confidence interval
df_sample <- sample_result[[2]]
sample_id <- df_sample[, 1]
y_sample <- y[sample_id]
estimated <- sbs_pps_estimate(
  population, sample_sizes, y_sample, df_sample,
  n_bootstrap = 100, alpha = 0.05
)
print(estimated)
