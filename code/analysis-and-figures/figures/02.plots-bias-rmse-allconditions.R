rm(list=ls())
library(tidyverse)
library(ggh4x)
library(ggdist)
source("code/analysis-and-figures/01.load-data.R")
source("code/analysis-and-figures/02.analysis-functions.R")

# Exclude the case of donors = 100
effects <- effects |> 
  filter(n_don != "don100")

#### Function to create same figure as figures 3 and 4, for all conditions

plot_distribution <- function(filter1, filter2, value1, value2, facet_row, facet_col, estimator, considered_metric){
  
  # Get variables for title
  label1 <- rlang::as_name(rlang::quo(expr= {{filter1}}))
  label2 <- rlang::as_name(rlang::quo(expr= {{filter2}}))
  label1 <- title_labeller(label1)
  label2 <- title_labeller(label2)
  
  # bias and rmse
  summaries <- effects |> 
    filter({{ filter1}}  == value1, {{ filter2}} == value2) |> 
    compute_differences(reference = considered_metric, pivot = TRUE, chosen_period = "post", solution = "Posible") |> 
    group_by(model, {{ facet_row }}, {{ facet_col }}) |>
    mutate(squared_error = error^2) |> 
    summarize(bias= mean(error),
              mse = mean(squared_error),
              rmse = sqrt(mse)) |> 
    filter(!str_detect(model, estimator))
  
  # whole distribution of effects
  data <- effects |> 
    filter({{ filter1}}  == value1, {{ filter2}} == value2) |> 
    compute_differences(reference = considered_metric, pivot = TRUE, chosen_period = "post", solution = "Posible") |> 
    filter(!str_detect(model, estimator)) 

   data |> 
    ggplot(aes(x = model, y = error, fill = model)) +
    geom_hline(yintercept = 0, linetype = 848, linewidth = 1, colour = "grey50") +
    stat_histinterval(point_interval = "mean_qi") +
    geom_text(data = summaries, aes(x = model, y = bias, label = paste0(round(bias,0), "\n", "(", round(rmse,0), ")")),
            nudge_x = 0.2,nudge_y = 5, lineheight = .8,size = 4) +
    facet_grid(rows = vars({{ facet_row }}), cols = vars({{ facet_col }}),
              labeller = grid_labeller) +
    labs(title = "Fixed conditions:",
        subtitle = glue::glue("{label1} ({value1}), and {label2} ({value2})")) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 18)) +
    labs(y = "Differences",
         x = "Model") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(limits = c(-200, 200)) 
}


# Iterate across all conditions    
estimators <- c("Mean", "Median")
considered_metrics <- c("counterfactual", "mean_counterfactual")
un_output_means <- unique(effects$output_means)
un_size_ob <- unique(effects$size_ob)
un_n_preint <- unique(effects$n_preint)
un_n_don <- unique(effects$n_don)[1:3]

  
# First set of plots (preintervention outcomes and number of donors in facet)
for(i in 1:length(un_output_means)){
  for(j in 1:length(un_size_ob)){
    for(k in 1:length(estimators)){
      for(l in 1:length(considered_metrics)){
        print(paste(i,j,k,l))
        try(plot <- plot_distribution(filter1 = output_means, filter2 = size_ob, 
                          value1 = un_output_means[i], value2 = un_size_ob[j], 
                          facet_row = n_preint, facet_col = n_don, 
                          estimator = estimators[k], considered_metric = considered_metrics[l]))
        try(
          ggsave(paste0("plots/causal_effects/", estimators[k], "/", "fig2_","estimator_", estimators[k],
                        "_metric_", considered_metrics[l], 
                        "_output_means_", un_output_means[i], 
                        "_size_ob_", un_size_ob[j], ".png"), plot, width = 15, height = 15)
        )
      }
    }
  }
}

# Second set of plots (changes which variables are rows and columns in facet)
for(i in 1:length(un_output_means)){
  for(j in 1:length(un_size_ob)){
    for(k in 1:length(estimators)){
      for(l in 1:length(considered_metrics)){
        print(paste(i,j,k,l))
        try(plot <- plot_distribution(filter1 = n_preint, filter2 = n_don, 
                                      value1 = un_n_preint[i], value2 = un_n_don[j], 
                                      facet_row = output_means, facet_col = size_ob, 
                                      estimator = estimators[k], considered_metric = considered_metrics[l]))
        try(
          ggsave(paste0("plots/causal_effects/", estimators[k], "/", "fig2_","estimator_", estimators[k],
                        "_metric_", considered_metrics[l], 
                        "_output_means_", un_output_means[i], 
                        "_size_ob_", un_size_ob[j], ".png"), plot, width = 15, height = 15)
        )
      }
    }
  }
}



