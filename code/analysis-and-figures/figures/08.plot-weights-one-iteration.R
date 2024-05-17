
# Figure 6:: Weights of different models
library(ggdist)
weights <- readRDS("processed_results/complete_simulation/weights.rds")

# Select one iteration
iteration_number <- 6000
# Replicate data for that iteration
iteration_data <- gen_dat(n_periods_pre = grid$n_periods_pre[iteration_number], 
                          n_periods_post = grid$n_periods_post[iteration_number], 
                          n_donors = grid$n_donors[iteration_number],
                          size_ob = grid$size_ob[iteration_number],
                          outcomes_used = grid$outcomes_used[iteration_number],
                          seed = grid$seed[iteration_number])

# Replicate models for that iteration
bayes_hs <- get_bayes_result(iteration_data, sample = TRUE, model = "horseshoe.stan",
                             seed = grid$seed[iteration_number])
bayes_simp <- get_bayes_result(iteration_data, sample = TRUE, model = "horseshoe_simplex.stan",
                               seed = grid$seed[iteration_number])
synth <- get_synth(iteration_data)


# Store posterior of weights for both bayesian models
weights_simp <- bayes_simp$draws() |> 
  posterior::as_draws_df() |> 
  select(starts_with("unit_")) |> 
  pivot_longer(cols = everything(),
               names_to = "weight",
               values_to = "value") |> 
  mutate(method = "Horseshoe Simplex") |> 
  mutate(unit_weights = gsub("unit_weights\\[(\\d+)\\]", "Unit \\1", weight)) |> 
  mutate(unit_weights = factor(unit_weights, levels = paste0("Unit ", 1:10)))

weight_hs <- bayes_hs$draws() |> 
  posterior::as_draws_df() |> 
  select(starts_with("unit_")) |>
  pivot_longer(cols = everything(),
               names_to = "weight",
               values_to = "value") |> 
  mutate(method = "Horseshoe") |> 
  mutate(unit_weights = gsub("unit_weights\\[(\\d+)\\]", "Unit \\1", weight)) |> 
  mutate(unit_weights = factor(unit_weights, levels = paste0("Unit ", 1:10)))

# Store estimates of Standard SCM
synth_weights <- weights |> 
  filter(row == 6000) |> 
  select(variable, standard_weights) |> 
  rename(weight = variable,
         value = standard_weights) |> 
  mutate(stat = "standard")  |> 
  mutate(unit_weights = gsub("unit_weights\\[(\\d+)\\]", "Unit \\1", weight)) |> 
  mutate(unit_weights = factor(unit_weights, levels = paste0("Unit ", 1:10)))


summaries_hs <- weight_hs |> 
  group_by(weight) |> 
  summarize(mean = mean(value),
            median = median(value)) |> 
  left_join(synth_weights, by = "weight") |> 
  pivot_longer(cols = c("mean", "median", "value"),
               names_to = "statistic",
               values_to = "value") 

summaries_simp <- weights_simp |> 
  group_by(weight) |> 
  summarize(mean = mean(value),
            median = median(value)) |> 
  left_join(synth_weights, by = "weight") |> 
  pivot_longer(cols = c("mean", "median", "value"),
               names_to = "statistic",
               values_to = "value") 

col_hs <- "navyblue"
col_simp <- "darkgreen"


plot_a <- weight_hs |> 
  ggplot(aes(y = unit_weights, x = value), fill = col_hs) +
  stat_slab(height = 1, normalize = "xy", expand = TRUE, colour = col_hs,
            fill = col_hs,
            alpha = 0.5) +
  geom_point(data = summaries_hs, aes(y = unit_weights, x = value, color = statistic), size = 3) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("mean" = "#CB2314", "median" = "#F0766A", "value" = "black"),
                     labels = c("Posterior Mean", "Posterior Median", "Standard SCM")) +
  theme_classic() +
  theme(text = element_text(size = 60),
        legend.title = element_blank(),
        legend.position = "top") +
  labs(y = NULL,
       x = NULL,
       title = "Horseshoe Priors BSCM")

plot_b <-  weights_simp |> 
  ggplot(aes(y = unit_weights, x = value), fill = col_simp) +
  stat_slab(height = 1, normalize = "xy", expand = TRUE, colour = col_simp,
            fill = col_simp,
            alpha = 0.5) +
  geom_point(data = summaries_simp, aes(y = unit_weights, x = value, color = statistic), size = 3) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("mean" = "#CB2314", "median" = "#F0766A", "value" = "black"),
                     labels = c("Posterior Mean", "Posterior Median", "Standard SCM")) +
  theme_classic() +
  theme(text = element_text(size = 60),
        legend.title = element_blank(),
        legend.position = "top") +
  labs(y = NULL,
       x = NULL,
       title = "Horseshoe Simplex Priors BSCM") 
 
library(patchwork)

figure6 <- plot_a + plot_b + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        title = element_text(size = 80, family = "serif"),
        axis.text = element_text(size = 70),
        axis.title = element_text(size = 76),
        legend.text = element_text(size = 76))
ggsave("plots/figure6a.png", plot_a, width = 8, height = 10, dpi = 400)
ggsave("plots/figure6b.png", plot_b, width = 8, height = 10, dpi = 400)
ggsave("plots/figure6.png", figure6, width = 16, height = 10, dpi = 400)