# Input: effects dataframe
# Output: dataframe with differences between estimates and different metrics
# Arguments: difference (different to counterfactual, differences in mean counterfactuals, or difference vs standard synt)
# Chosen period: filter (post or pre) intervention time points
# Solution: filter data according a convex weighted average of the treated units
# is possible or not

compute_differences <- function(data, 
                                reference = "counterfactual",
                                chosen_period = "post",
                                solution = "Posible",
                                pivot = TRUE) {
  
  # Do nothing if solution = "both". Otherwise, filter out either data for which
  # synthetic control is possible or impossible
  if (solution == "both") {
    
  } else if (solution %in% c("Posible", "Imposible")) {
    data <- data %>%
      left_join(posible_synt) %>%
      filter(possible == solution)
  }
  
  # Compute differences between estimates and counterfactual (at each time point)
  # or between estimate and mean counterfactual (mean counterfactual across all posintervention time points)
  # mean_conterfactual is included only for robustness checks
  if (reference == "counterfactual") {
    data <- data %>%
      filter(period == chosen_period) %>%
      mutate(
        d_st = counterfactual - counterfactual_standard,
        d_hs_mean = counterfactual - mean_hs,
        d_simp_mean = counterfactual - mean_simp,
        d_hs_median = counterfactual - median_hs,
        d_simp_median = counterfactual - median_simp
      )
  } else if (reference == "mean_counterfactual") {
    data <- data %>%
      filter(period == chosen_period) %>%
      group_by(file_name) %>%
      summarize(
        counterfactual_mean = mean(counterfactual),
        standard_mean = mean(counterfactual_standard),
        hs_mean_mean = mean(mean_hs),
        hs_median_mean = mean(median_hs),
        simp_mean_mean = mean(mean_simp),
        simp_median_mean = mean(median_simp)
      ) %>%
      mutate(
        d_st = counterfactual_mean - standard_mean,
        d_hs_mean = counterfactual_mean - hs_mean_mean,
        d_hs_median = counterfactual_mean - hs_median_mean,
        d_simp_mean = counterfactual_mean - simp_mean_mean,
        d_simp_median = counterfactual_mean - simp_median_mean
      )
    
    data <- data %>%
      left_join(conditions, by = "file_name")
  }
  # Pivot some columns in longer format, so that's easier to work with ggplot
  if (pivot) {
    data <-data %>%
      pivot_longer(cols = starts_with("d"),
                   names_to = "model",
                   values_to = "error") |> 
      mutate(
        model = case_when(
          model == "d_simp_mean" ~ "Horseshoe Simplex Mean",
          model == "d_st" ~ "Standard Synthetic Control",
          model == "d_simp_median" ~ "Horseshoe Simplex Median",
          model == "d_hs_mean" ~ "Horseshoe Mean",
          model == "d_hs_median" ~ "Horseshoe Median"
        )
      )
  }
  return(data)
}  

## Labeller function for plots
grid_labeller <- as_labeller(c(`outnone` = "Only covariates", 
                             `out5means` = "5 preintervention outcomes + covariates",
                             `outall` = "All preintervention outcomes + covariates",
                             `preint5` = "5 preintervention time points",
                             `preint10` = "10 preintervention time points",
                             `preint20` = "20 preintervention time points",
                             `don50` = "50 donors",
                             `don10` = "10 donors",
                             `don20` = "20 donors",
                             `20` = "20 % explained by observed covariates",
                             `45` = "45 % explained by observed covariates",
                             `70` = "70 % explained by observed covariates"),
                           default = label_wrap_gen(width = 25))

### Title function for plots

title_labeller <- function(variable){
  if(variable == "n_don"){
    label <- "number of donors"
  } else if(variable == "n_preint"){
    label <- "number of preintervention periods"
  } else if(variable == "output_means"){
    label <- "pretreatment outcomes considered"
  } else if(variable == "size_ob"){
    label <- "percentage of variance explained by observed covariates"
  }
  return(label)
}


