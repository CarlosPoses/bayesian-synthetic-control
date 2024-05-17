library(dplyr)
library(cmdstanr)
library(Synth)
library(stringr)
library(parallel)
library(pbapply)

source("code/run-simulation/01.gen-dat-function.R")
source("code/run-simulation/02.estimation-functions.R")
source("code/run-simulation/03.error-handlers.R")
source("code/run-simulation/04.sim-functions.R")

# Set seed for reproducibility
set.seed(333)

# Give model(s) names
sim_models <- c("horseshoe_simplex.stan", "horseshoe.stan")
folder_name <- c("complete_simulation") # date I launched the simulation

grid <- expand.grid(iter = 1:1000,
                    n_periods_pre = c(5,10,20), 
                    n_periods_post = 10, 
                    n_donors = c(10, 20, 50, 100), 
                    size_ob = c(20, 45, 70),
                    outcomes_used = c("5means", "none", "all")) %>% 
  arrange(n_donors) %>% 
  filter(!(n_donors %in% c(50,100) & (size_ob %in% c(20, 70) |
           n_periods_pre %in% c(5, 10) |
           outcomes_used %in% c("all")))) 

grid$seed <- sample(1e7, size = nrow(grid), replace = FALSE)

saveRDS(grid, paste0("sim-output/", folder_name, "/grid/grid.rds"))

# Make clusters and exports
clus <- makeForkCluster(40) # select number of cores available in your computer. 
# If your computer is not MAC or Linux, you can use parallel computation using the commented
# code velow

# # Loaded functions
# loaded.functions <- ls.str()
# attributes(loaded.functions) <- NULL
# clus <- makeCluster(7) 
# clusterExport(clus, loaded.functions)
# clusterEvalQ(clus, {
#   library(Synth)
#   library(pensynth)
#   library(cmdstanr)
#   library(dplyr)
#   library(stringr)})

# # Apply simulation
pbsapply(1:nrow(grid), run_simulation, folder = folder_name, model_names = sim_models, cl = clus)
stopCluster(clus)
rm(clus)


