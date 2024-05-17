effects <- readRDS("sim-output/processed_results/effects.rds")
source("code/analysis-and-figures/02.analysis-functions.R")

library(tidyverse)


# compute coverage per condition  
effects <- effects |> 
  filter(n_don != "don100")
coverage <- effects |>  
  mutate(covered_hs = ifelse(between(counterfactual, q5_hs, q95_hs), TRUE, FALSE),
         covered_simp = ifelse(between(counterfactual, q5_simp, q95_simp), TRUE, FALSE))

# function to plot coverage, applied later to all conditions
plot_coverage <- function(filter1, filter2, value1, value2, facet_row, facet_col){
  
  # Get variables for title
  label1 <- rlang::as_name(rlang::quo(expr= {{filter1}}))
  label2 <- rlang::as_name(rlang::quo(expr= {{filter2}}))
  label1 <- title_labeller(label1)
  label2 <- title_labeller(label2)
  
  coverage |> 
    filter({{ filter1}}  == value1, {{ filter2}} == value2) |> 
    mutate(covered_hs = ifelse(between(counterfactual, q5_hs, q95_hs), TRUE, FALSE),
           covered_simp = ifelse(between(counterfactual, q5_simp, q95_simp), TRUE, FALSE)) |> 
    pivot_longer(cols = c(covered_hs, covered_simp), names_to = "model", values_to = "covered") |> 
    group_by(model, {{ facet_row}}, {{ facet_col }}) |> 
    summarize(mean = mean(covered)) |> 
    ggplot(aes(x = model, y = mean, fill = model)) +
    geom_point() +
    geom_text(aes(label = round(mean, 2)), hjust = -0.2) + 
    facet_grid(rows = vars({{ facet_row }}), cols = vars({{ facet_col }}),
               labeller = grid_labeller) +
    labs(title = "Fixed conditions:",
         subtitle = glue::glue("{label1} ({value1}), and {label2} ({value2})")) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 18)) +
    scale_y_continuous(limits = c(0, 1))
  
}

un_output_means <- unique(effects$output_means)
un_size_ob <- unique(effects$size_ob)

# Save plots for al conditions
for(i in 1:length(un_output_means)){
  for(j in 1:length(un_size_ob)){
        try(plot <- plot_coverage(filter1 = output_means, filter2 = size_ob, 
                                      value1 = un_output_means[i], value2 = un_size_ob[j], 
                                      facet_row = n_preint, facet_col = n_don))
        try(
          ggsave(paste0("plots/coverages/", "_output_means_", un_output_means[i], 
                        "_size_ob_", un_size_ob[j], ".png"), plot, width = 15, height = 15)
        )
      }
    }


