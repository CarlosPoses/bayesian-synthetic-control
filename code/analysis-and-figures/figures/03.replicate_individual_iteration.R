library(tidyverse)
library(Synth)
library(cmdstanr)
library(bayesplot)
source("code/analysis-and-figures/01.load-data.R")
source("code/analysis-and-figures/02.analysis-functions.R")
source("code/run-simulation/01.gen-dat-function.R")
source("code/run-simulation/02.estimation-functions.R")
source("code/run-simulation/03.error-handlers.R")

# Replicate one iteration

grid <- readRDS("sim-output/complete_simulation/grid/grid.rds")

# Select one iteration
# 6000 old one, 5988, 5650
iteration_number <- 5750
# Replicate data for that iteration
iteration_data <- gen_dat(n_periods_pre = grid$n_periods_pre[iteration_number], 
                 n_periods_post = grid$n_periods_post[iteration_number], 
                 n_donors = grid$n_donors[iteration_number],
                 size_ob = grid$size_ob[iteration_number],
                 outcomes_used = grid$outcomes_used[iteration_number],
                 seed = grid$seed[iteration_number])

# Replicate models for that iteration
bayes_hs <- get_bayes(iteration_data, sample = TRUE, model = "horseshoe.stan",
                   seed = grid$seed[iteration_number])
bayes_hs <- get_bayes_result(iteration_data, sample = TRUE, model = "horseshoe.stan",
                   seed = grid$seed[iteration_number])
bayes_simp <- get_bayes_result(iteration_data, sample = TRUE, model = "horseshoe_simplex.stan",
                   seed = grid$seed[iteration_number])
synth <- get_synth(iteration_data)
synth_automatic_weights <- get_synth(iteration_data, custom.v = FALSE)
bayes_hs_summary <- bayes_hs$summary()
bayes_simp_summary <- bayes_simp$summary()


# Store outcomes of different models
outcomes <- cbind(iteration_data$Y1, iteration_data$Y0) %>%
  as.data.frame() %>%
  mutate(bayes_simp = bayes_hs_summary %>% filter(str_detect(variable, "synt")) %>% select(mean) %>% pull(),
         bayes_hs = bayes_simp_summary %>% filter(str_detect(variable, "synt")) %>% select(mean) %>% pull(),
         synth = iteration_data$Y0 %*% synth$solution.w[,1],
         synth_auto = iteration_data$Y0 %*% synth_automatic_weights$solution.w[,1],
         time = 1:nrow(iteration_data$Y1))   


#### Figure 2 ##############

plot_outcomes <- outcomes %>% 
  pivot_longer(cols = -time,
               names_to = "method",
               values_to = "value") %>%
  filter(str_starts(method, "V")) |> 
  mutate(unit = ifelse(method == "V1", "treated", "untreated_units")) |> 
  mutate(method = fct_relevel(method, "V1", after = 10L))

figure2 <- ggplot() +
    geom_line(data = plot_outcomes, 
            aes(x = time, y = value[,1], colour = unit, group = method, linetype = unit),
            linewidth = 1.3) +
    geom_vline(xintercept = (grid$n_periods_pre[iteration_number] + 1), linetype = "dashed") +
    theme_classic() + 
    scale_color_manual(values = c("#CB2314", "grey90"), labels = c("Treated Unit", "Untreated Units")) +
    scale_linetype_manual(values = c("solid", 71), labels = c("Treated Unit", "Untreated Units")) +
  scale_y_continuous(limits = c(400, 1075)) +
  scale_x_continuous(limits = c(1,30), breaks = seq(3,30, by = 3)) +
  theme(legend.text = element_text(size = 70),
        legend.position = "top",
        text = element_text(size = 70),
        axis.title = element_text(size = 76, margin = margin(t = 30, r = 10, b = 10, l = 10)),
        legend.title = element_blank()) +
  annotate(geom = "text", x = 23, y = 470, label = "Y['1t']^I", colour = "#CB2314",
           size = 24,
           family = "serif",
           alpha = 1,
           parse = TRUE) +
  annotate(geom = "text", x = 23.75, y = 470, label = "=", colour = "#CB2314",
           size = 24,
           family = "serif") +
  annotate(geom = "text", x = 24.5, y = 470, label = "Y['1t']^N", colour = "#CB2314",
           size = 20,
           family = "serif",
           alpha = 1,
           parse = TRUE) +
  labs(y = "Outcome",
       x = "Time")

ggsave(filename = "plots/manuscript_figures/figure2.png", plot = figure2, width = 18, height = 8)


## Figure 3 ##############

plot_methods <- outcomes %>% 
  pivot_longer(cols = -time,
               names_to = "method",
               values_to = "value") %>%
  filter(method %in% c("bayes_simp", "bayes_hs")) |> 
  rename(estimator = method)

credibility_intervals <- bayes_hs_summary |> 
  left_join(bayes_simp_summary, by = "variable", suffix = c("_hs","_simp")) |> 
  filter(variable %>% str_detect("synt")) |> 
  select(starts_with("q"), variable, mean_hs, mean_simp) |> 
  mutate(time = 1:30)

col_hs <- "navyblue"
col_simp <- "darkgreen"

figure3a <- figure2 +
  geom_ribbon(data = credibility_intervals |> filter(time > 20), 
              aes(x = time, y = mean_hs, ymin = q5_hs, ymax = q95_hs),
              colour = col_hs, alpha = 0.5,
              fill = col_hs) +
  geom_line(data = credibility_intervals,
            aes(x = time, y = mean_hs),
            colour = col_hs,
            linewidth = 1.3) +
  labs(title = "Horseshoe Priors BSCM")

figure3b <- figure2 +
  geom_ribbon(data = credibility_intervals |> filter(time > 20), 
              aes(x = time, y = mean_simp, ymin = q5_simp, ymax = q95_simp),
              fill = col_simp, alpha = 0.5,
              colour = col_simp) +
  geom_line(data = credibility_intervals,
            aes(x = time, y = mean_simp),
            colour = col_simp,
            linewidth = 1.3) +
  labs(title = "Horseshoe Simplex Priors BSCM")

library(patchwork) 
figure3 <- figure3a / figure3b + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        title = element_text(size = 80, family = "serif"),
        axis.text = element_text(size = 60),
        axis.title = element_text(size = 70),
        legend.title = element_blank())
ggsave(plot = figure3a, filename = "plots/manuscript_figures/figure3a.png", width = 18, height = 8)
ggsave(plot = figure3b, filename = "plots/manuscript_figures/figure3b.png", width = 18, height = 8)
ggsave(plot = figure3, filename = "plots/manuscript_figures/figure3.png", width = 16, height = 14)


