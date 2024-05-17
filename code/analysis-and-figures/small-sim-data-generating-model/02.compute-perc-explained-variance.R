source("figure-out-dgm/functions.R")
library(tidyverse)

## Run the simulation with some conditions
set.seed(323)
summary1 <- summary_across_iterations(n_iterations = 1000,
                                      n_units = 30,
                                      n_obs_cov = 5, n_unobs_cov = 5,
                                      sd_error_term = 35,
                                      min_obs = 5,
                                      max_obs = 10,
                                      min_unobs = 5,
                                      max_unobs = 10)

summary2 <- summary_across_iterations(n_iterations =1000,
                                      n_units = 30,
                                      n_obs_cov = 5, n_unobs_cov = 5,
                                      sd_error_term = 40,
                                      min_obs = 5,
                                      max_obs = 10,
                                      min_unobs = 10,
                                      max_unobs = 15)

summary3 <- summary_across_iterations(n_iterations = 1000,
                                      n_units = 30,
                                      n_obs_cov = 5, n_unobs_cov = 5,
                                      sd_error_term = 40,
                                      min_obs = 10,
                                      max_obs = 15,
                                      min_unobs = 5,
                                      max_unobs = 10)

saveRDS(summary1, "code/analysis-and-figures/small-sim-data-generating-model/summary1.rds")
saveRDS(summary2, "code/analysis-and-figures/small-sim-data-generating-model/summary2.rds")
saveRDS(summary3, "code/analysis-and-figures/small-sim-data-generating-model/summary3.rds")
summary1
summary2
summary3
