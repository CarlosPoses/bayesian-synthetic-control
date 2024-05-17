rm(list = ls())
library(tidyverse)
library(ggh4x)
library(ggdist)
source("code/analysis-and-figures/01.load-data.R")
source("code/analysis-and-figures/02.analysis-functions.R")

effects <- readRDS("sim-output/processed_results/weights.rds")
# Compute weights higher than threshold for all values
weights_plot <- 
  weights |> 
  filter(n_don != "don100") |> 
  select(variable, median_hs, median_simp,mean_hs, mean_simp, standard_weights, file_name) |> 
  mutate(`> 0.03` = 0.03,
         `> 0.02` = 0.02,
         `> 0.05` = 0.05,
         `> 0.10` = 0.10) |> 
  pivot_longer(cols = starts_with(">"),
               names_to = "threshold", values_to = "threshold_value") |> 
  pivot_longer(cols = c("median_hs", "median_simp", "mean_hs", "mean_simp", "standard_weights"),
               names_to = "model", values_to = "estimate") |> 
  mutate(estimate = abs(estimate)) |> 
  mutate(above_threshold = if_else(estimate > threshold_value, TRUE, FALSE)) |> 
  group_by(file_name, threshold, model) |> 
  summarize(count_above_threshold = sum(above_threshold), # change to mutate if you want to keep all rows, e.g., for checking code
            count_below_threshold = sum(!above_threshold)) |> 
  left_join(conditions, by = "file_name") 


# Plot loop for supplementary materials
un_preint <- unique(conditions$n_preint)
un_out_means <- unique(conditions$output_means)
un_size_ob <- unique(conditions$size_ob)
estimator <- c("mean", "median")

for(i in 3:length(un_preint)){
  for(j in 3:length(un_out_means)){
    for(k in 1:length(un_size_ob)){
      for(l in 1:length(estimator)){
        
  data <- weights_plot |> 
  filter(n_preint == un_preint[i],
         output_means == un_out_means[j],
         size_ob == un_size_ob[k]) |> 
  filter(!stringr::str_detect(model, estimator[l])) 
  
  plot <- data |> 
  ggplot(aes(x = count_above_threshold, fill = model, colour = model, group = model)) +
  geom_histogram(alpha = 0.5,
                 position = position_dodge(),
                 binwidth = 1) +
  facet_grid(cols = vars(threshold),
             rows = vars(n_don)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  scale_x_continuous(guide = guide_axis(n.dodge=2),
                       breaks = seq(0, 20, by = 1),
                     ) +
  theme_bw() + 
  theme(text = element_text(size = 16)) +
  labs(title = paste("Preintervention N:", un_preint[i], "Outcomes uses:", un_out_means[j], "Percentage explained:", un_size_ob[k]),
       subtitle = paste("Estimator:", estimator[l])) 
  
  ggsave(paste("plots/weights_sparsity/sparsity_plot_", un_preint[i], "_", un_out_means[j], "_", un_size_ob[k], "_", estimator[l], ".png"), plot,
         height = 10, width = 15)
  print(paste(i, j, k, l))
    }
    }
  }
}

  
