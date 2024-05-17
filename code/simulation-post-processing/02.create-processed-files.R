library(parallel)
library(pbapply)
library(tidyverse)
source("code/analysis/01.process-files-function.R")

# List all files in the simulation output folder
files <- list.files("sim-output/complete_simulation")[!grepl("grid", list.files("sim-output/complete_simulation/"))]

# Detect files that threw an error:
## Loop over all files, and return true if the length of the standard synthetic control is 1.
## (This length is only one when there was an error)
files_error <- rep(NA, length(files))

for(i in seq_along(files)){
  file.name <- files[i]
  model <- readRDS(paste0("sim-output/complete_simulation/", file.name))
  if(length(model$weights_standard) == 1){
    files_error[i] <- TRUE
  } else {
    files_error[i] <- FALSE
  }
}

# Excludes files that threw an error
files <- files[!files_error]

## Create different datasets:
# Make clusters and exports

clus <- makeCluster(7) # Remember to change cluster

clusterExport(clus, "process_files")
clusterEvalQ(clus, {
  library(stringr)
  library(dplyr)
  library(tidyr)})

# Apply the function to each row of the grid, and combine into a dataframe

conditions <- pblapply(files, process_files, out = "conditions", cl = clus) #@ASK takes some time to process
conditions <- dplyr::bind_rows(conditions) 

weights <- pblapply(files, process_files, out = "weights", cl = clus)
weights <- dplyr::bind_rows(weights)

effects <- pblapply(files, process_files, out = "effects", cl = clus)
effects <- dplyr::bind_rows(effects)

covariates <- pblapply(files, process_files, out = "covariates", cl = clus)
covariates <- dplyr::bind_rows(covariates)

pretreatment_outcomes <- pblapply(files, process_files, out = "pretreatment_outcomes", cl = clus)
pretreatment_outcomes <- dplyr::bind_rows(pretreatment_outcomes)

mess_and_diag <- pblapply(files, process_files, out = "mess_and_diag", cl = clus)

stopCluster(clus)
rm(clus)

# Modify and merge some of these datasets.
# 1) Convert all conditions to factors, 2) give correct order of factor levels

conditions <- conditions |>
mutate(n_preint = paste0("preint", n_preint),
       n_preint = as_factor(n_preint),
       n_don = paste0("don", n_don),
       n_don = as_factor(n_don),
       output_means = as_factor(output_means),
       size_ob = as_factor(size_ob)) |>
  mutate(n_preint = forcats::fct_relevel(n_preint, "preint5", "preint10", "preint20"),
         n_don = forcats::fct_relevel(n_don, "don10", "don20", "don50"),
         output_means = forcats::fct_relevel(output_means, "outall", "out5means", "outnone"),
         size_ob = forcats::fct_relevel(size_ob, "20", "45", "70"))

# Join weights and effects with conditions
weights <- weights %>% 
  left_join(conditions, by = "file_name") 
effects <- effects %>%
  left_join(conditions, by = "file_name") 



## Creata dataframe of possible synthetic controls
# Compute outcome mean, for each unit, across time
outcome_means <- pretreatment_outcomes %>% 
  group_by(file_name, unit) %>% 
  mutate(outcome_mean = mean(value)) %>% 
  select(-time, -value) %>% 
  unique()

# Compute maximum and minimum outcome mean for each file
outcome_max <- outcome_means %>% 
  group_by(file_name) %>% 
  summarize(max = max(outcome_mean),
            min = min(outcome_mean))

# For treated units whose mean outcome is the maximum or minimum, synthetic control is not possible
# Flag it in a dataframe
posible_synt <- outcome_means %>% 
  left_join(outcome_max) %>%
  filter(unit == "treated") %>% 
  mutate(possible = ifelse(outcome_mean == max | outcome_mean == min, "Imposible", "Posible")) %>% 
  select(file_name, possible)

### Save all files
saveRDS(conditions, "sim-output/processed_results/conditions.rds")
saveRDS(weights, "sim-output/processed_results/weights.rds")
saveRDS(effects, "sim-output/processed_results/effects.rds")
saveRDS(covariates, "sim-output/processed_results/covariates.rds")
saveRDS(mess_and_diag, "sim-output/processed_results/mess_and_diag.rds")
saveRDS(pretreatment_outcomes, "sim-output/processed_results/pretreatment_outcomes.rds")
saveRDS(posible_synt, "sim-output/processed_results/possible_synt.rds")
