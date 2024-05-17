library(tidyverse)
data <- readRDS("sim-output/processed_results/possible_synt.rds")
conditions <- readRDS("sim-output/processed_results/conditions.rds")
data <- data |> 
  left_join(conditions, by = "file_name")
# Count size of simulation condition, by conditions
data |> 
  filter(possible == "Posible") |> 
  group_by(n_preint, n_don, output_means, size_ob) |> 
  count() |> 
  print(n = Inf) |> 
  arrange(n)
