rm(list=ls())
showtext::showtext_auto()
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

california <- readRDS("data/proposition99.rds")
# Function to create X0, etc
preprocess_data <- function(model, covariates = TRUE){
covariates <- california |> 
  filter(year <= 1988,
         year >= 1980) |> 
  select(-year, -cigsale) |> 
  group_by(state) |> 
  mutate(across(everything(), ~mean(.x, na.rm = TRUE))) |> 
  unique() |> 
  pivot_longer(
    cols = c(age15to24, lnincome, retprice, beer), names_to= "variable", values_to = "value") 

if(model == "synth"){
lags <- california |> 
  select(cigsale, year, state) |> 
  filter(year == 1975 | year == 1980 | year == 1988) |> 
  mutate(variable = paste0("cigsale", year),
         value = cigsale) |> 
  select(-cigsale, -year)
  X <- rbind(covariates, lags) 
  
  } else if(model == "bayes"){
  lags <- california |>
      select(cigsale, year, state) |> 
      filter(year < 1989) |> 
      mutate(variable = paste0("cigsale", year),
             value = cigsale) |>
      select(-cigsale, -year, state, variable, value)
      X <- rbind(lags)
    }

X1 <- X |> filter(state == "California") |> select(-state)
X0 <- X |> filter(state != "California") 
X1 <- as.matrix(X1$value, ncol = 1)

X0 <- X0 |> pivot_wider(names_from = state,
              values_from = value)  |> 
  select(-variable) |> 
  as.matrix() 

X_all <- cbind(X1, X0)
sc <- apply(X_all, 1, sd)
X1 <- X1/sc
X0 <- X0/sc
  
Y0 <- california |> 
  filter(state != "California") |> 
  select(cigsale, state, year) |> 
  pivot_wider(names_from = state, values_from = cigsale) |> 
  select(-year) |>
  as.matrix()

Y0 <- Y0[, sort(colnames(X0))]
Y1 <- california |>
  filter(state == "California") |> 
  select(cigsale) |> 
  as.matrix(ncol = 1)
v <- rep(1, nrow(X1))

X0 <- X0[, sort(colnames(X0))]

Y0_pre <- Y0[1:19, ]
Y1_pre <- Y1[1:19, ] |> as.matrix(ncol = 1)
Y0_post <- Y0[20:nrow(Y0), ]
Y1_post <- Y1[20:nrow(Y1), ]
iteration_data <- list(Y0 = Y0, Y1 = Y1, 
                       X0 = X0, X1 = X1, v = v, 
                       Y0_pre = Y0_pre, Y1_pre = Y1_pre, 
                       Y0_post = Y0_post, Y1_post = Y1_post)
return(iteration_data)
}

# Create data for each model
data_bayes <- preprocess_data("bayes")
data_synth <- preprocess_data("synth")


# Replicate models 
bayes_hs <- get_bayes_result(data_bayes, sample = TRUE, model = "horseshoe.stan",
                   seed = 123)
bayes_simp <- get_bayes_result(data_bayes, sample = TRUE, model = "horseshoe_simplex.stan",
                   seed = 123)
synth_auto <- get_synth(data_synth, custom.v = FALSE)

# Get results from bayes_results 
bayes_hs_summary <- bayes_hs$summary()
bayes_simp_summary <- bayes_simp$summary()
bayes_results <- bayes_hs_summary |> 
  left_join(bayes_simp_summary, by = "variable", suffix = c("_hs", "_simp")) 

weights_simp <- bayes_simp_summary %>% filter(str_detect(variable, "unit_weight")) 

## Plot outcomes
plot_outcomes_data <- bayes_results |> 
  filter(str_detect(variable, "synt")) |>
  mutate(standard_auto = data_synth$Y0 %*% synth_auto$solution.w[,1],
         time = 1970:2000,
         treated_unit = data_synth$Y1) |> 
  pivot_longer(cols = starts_with(c("mean", "standard", "treated")),
               names_to = "method",
               values_to = "value") 

data_synth$Y0 %*% synth_auto$solution.w[,1]

annotation_size <- 20

# Plot SIMPLEX
data <- plot_outcomes_data |> filter(method != "mean_hs") |> 
  mutate(method = fct_relevel(method, c("mean_simp", "standard_auto", "treated_unit")))

figure7 <- ggplot(data) +
  geom_ribbon(data = plot_outcomes_data |> select(value, q5_simp, q95_simp, time, method),
              aes(x = time, ymin = q5_simp, ymax = q95_simp),
              fill = "#FFD700",
              alpha = 0.5) +
    geom_line(aes(x = time, y = value, group = method, colour = method),
              linewidth = 1.1,
              alpha = 0.8) +
    scale_color_manual(values = c("mean_simp" = "#FFD700", "standard_auto" = "black", "treated_unit" = "#CB2314"),
                     labels = c("Horseshoe Simplex BSCM", "Standard SCM", "California")) +
  # themes
    theme_classic() +
    theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 76),
        axis.text = element_text(size = 70),
        legend.text = element_text(size = 70, family = "serif"),
        text = element_text(size = 70),
        plot.caption = element_text(size = 12, face = "italic"),
        plot.margin = margin(2,2,0,0, unit = "cm")) +
  guides(colour = guide_legend(nrow = 1)) +
    labs(y = "Cigarette sales*",
       x = NULL,
       caption = "*Packs per capita") +
  scale_x_continuous(breaks = seq(1970, 2000, by = 5)) +
  coord_cartesian(ylim = c(0,200),
                  xlim = c(1970, 2000),
                  clip = "off") +
  annotate(geom = "text", x = 1985.2, y = 190, label = "Anti-tobacco \n law",
           size = annotation_size,
           lineheight = 0.3,
           family = "serif") +
  annotate(geom = "text", x = 1987.5, y = 183, label = expression(plain("(") * T[0] * plain(")")),
           size = annotation_size,
           family = "serif",
           parse = TRUE) +
  geom_curve(arrow = arrow(length = unit(0.03, "npc")),
             curvature = 0.5,
             colour = "grey40",
             aes(x = 1986, y = 178, xend = 1988.5, yend = 165)) +
  ## California
  annotate(geom = "text", x = 1976, y = 110, 
           label = expression(plain("California ") * plain("(") * Y['1t'] * plain(")")),
           colour = "#CB2314",
           size = annotation_size,
           family = "serif",
           alpha = 1,
           parse = TRUE) +
  annotate(geom = "text", x = 1995, y = 40, label = "Y['1t']^I", colour = "#CB2314",
           size = annotation_size,
           family = "serif",
           alpha = 1,
           parse = TRUE) +
  geom_vline(xintercept = 1989, linetype = "dashed") 

ggsave("plots/manuscript_figures/figure7.png", figure7, width = 9, height = 7)

## Plot Horseshoe

# Plot SIMPLEX
data <- plot_outcomes_data |> filter(method != "mean_simp") |> 
  mutate(method = fct_relevel(method, c("mean_hs", "standard_auto", "treated_unit")))

figure7_appendix <- ggplot(data) +
  geom_ribbon(data = plot_outcomes_data |> select(value, q5_hs, q95_hs, time, method),
              aes(x = time, ymin = q5_hs, ymax = q95_hs),
              fill = "#FFD700",
              alpha = 0.5) +
  geom_line(aes(x = time, y = value, group = method, colour = method),
            linewidth = 1.1,
            alpha = 0.8) +
  scale_color_manual(values = c("mean_hs" = "#FFD700", "standard_auto" = "black", "treated_unit" = "#CB2314"),
                     labels = c("Horseshoe BSCM", "Standard SCM", "California")) +
  # themes
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 76),
        axis.text = element_text(size = 70),
        legend.text = element_text(size = 70, family = "serif"),
        text = element_text(size = 70),
        plot.caption = element_text(size = 12, face = "italic"),
        plot.margin = margin(2,2,0,0, unit = "cm")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(y = "Cigarette sales*",
       x = NULL,
       caption = "*Packs per capita") +
  scale_x_continuous(breaks = seq(1970, 2000, by = 5)) +
  coord_cartesian(ylim = c(0,200),
                  xlim = c(1970, 2000),
                  clip = "off") +
  annotate(geom = "text", x = 1985.2, y = 190, label = "Anti-tobacco \n law",
           size = annotation_size,
           lineheight = 0.3,
           family = "serif") +
  annotate(geom = "text", x = 1987.5, y = 183, label = expression(plain("(") * T[0] * plain(")")),
           size = annotation_size,
           family = "serif",
           parse = TRUE) +
  geom_curve(arrow = arrow(length = unit(0.03, "npc")),
             curvature = 0.5,
             colour = "grey40",
             aes(x = 1986, y = 178, xend = 1988.5, yend = 165)) +
  ## California
  annotate(geom = "text", x = 1976, y = 110, 
           label = expression(plain("California ") * plain("(") * Y['1t'] * plain(")")),
           colour = "#CB2314",
           size = annotation_size,
           family = "serif",
           alpha = 1,
           parse = TRUE) +
  annotate(geom = "text", x = 1995, y = 40, label = "Y['1t']^I", colour = "#CB2314",
           size = annotation_size,
           family = "serif",
           alpha = 1,
           parse = TRUE) +
  geom_vline(xintercept = 1989, linetype = "dashed") 

ggsave("plots/manuscript_figures/figure7_appendix.png", figure7_appendix, width = 9, height = 7)


######## Plot weights

states_names <- colnames(data_bayes$X0)

weights_simp <- bayes_simp$draws() |> 
  posterior::as_draws_df() |> 
  select(starts_with("unit_")) |> 
  pivot_longer(cols = everything(),
               names_to = "weight",
               values_to = "value") |> 
  mutate(method = "Horseshoe Simplex") |> 
  mutate(unit_weights = gsub("unit_weights\\[(\\d+)\\]", "Unit \\1", weight)) |> 
  mutate(unit_weights = factor(unit_weights, levels = paste0("Unit ", 1:10)),
  state_names = rep(states_names, times = (1520000/length(states_names))))

weight_hs <- bayes_hs$draws() |> 
  posterior::as_draws_df() |> 
  select(starts_with("unit_")) |>
  pivot_longer(cols = everything(),
               names_to = "weight",
               values_to = "value") |> 
  mutate(method = "Horseshoe") |> 
  mutate(unit_weights = gsub("unit_weights\\[(\\d+)\\]", "Unit \\1", weight)) |> 
  mutate(unit_weights = factor(unit_weights, levels = paste0("Unit ", 1:10)),
  state_names = rep(states_names, times = (1520000/length(states_names))))

synth_weights <- data.frame(weights = synth_auto$solution.w) |> 
  mutate(unit = 1:length(w.weight)) |> 
  mutate(weight = paste0("unit_weights[", unit, "]"),
         state_names = states_names)


summaries_hs <- weight_hs |> 
  group_by(weight) |> 
  summarize(mean = mean(value),
            median = median(value)) |> 
  left_join(synth_weights, by = "weight") |> 
  pivot_longer(cols = c("mean", "median", "w.weight"),
               names_to = "statistic",
               values_to = "value") 

summaries_simp <- weights_simp |> 
  group_by(weight) |> 
  summarize(mean = mean(value),
            median = median(value)) |> 
  left_join(synth_weights, by = "weight") |> 
  pivot_longer(cols = c("mean", "median", "w.weight"),
               names_to = "statistic",
               values_to = "value") 

col_hs <- "navyblue"
col_simp <- "darkgreen"


figure_8_app <- weight_hs |> 
  filter(state_names %in% c("Utah", "Montana", "Nevada", "Colorado", "Connecticut", "Ohio", "Mississippi", "Illinois",
                            "West Virginia", "New Hampshire")) |> 
  ggplot(aes(y = state_names, x = value), fill = col_hs) +
  stat_slab(height = 1, normalize = "xy", expand = TRUE, colour = col_hs,
            fill = col_hs,
            alpha = 0.5) +
  geom_point(data = summaries_hs |> filter(state_names %in% c("Utah", "Montana", "Nevada", "Colorado", "Connecticut", "Ohio", "Mississippi", "Illinois",
                              "West Virginia")), aes(y = state_names, x = value, color = statistic), size = 3) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_color_manual(values = c("mean" = "#CB2314", "median" = "#F0766A", "w.weight" = "black"),
                     labels = c("Posterior Mean", "Posterior Median", "Standard SCM")) +
  theme_classic() +
  theme(text = element_text(size = 60),
        legend.title = element_blank(),
        legend.position = "top") +
  labs(y = NULL,
       x = NULL)

library(ggdist)
figure_8 <- weights_simp |> 
  filter(state_names %in% c("Utah", "Montana", "Nevada", "Colorado", "Connecticut", "Ohio",
                            "West Virginia", "New Hampshire")) |>
  ggplot(aes(y = state_names, x = value), fill = col_hs) +
  stat_slab(height = 1, normalize = "xy", expand = TRUE, colour = col_hs,
            fill = col_hs,
            alpha = 0.5) +
  geom_point(data = summaries_simp |> filter(state_names %in% c("Utah", "Montana", "Nevada", "Colorado", "Connecticut", "Ohio",
                                                              "West Virginia", "New Hampshire")), aes(y = state_names, x = value, color = statistic), size = 3) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("mean" = "#CB2314", "median" = "#F0766A", "w.weight" = "black"),
                     labels = c("Posterior Mean", "Posterior Median", "Standard SCM")) +
  theme_classic() +
  theme(text = element_text(size = 60),
        legend.title = element_blank(),
        legend.position = "top") +
  labs(y = NULL,
       x = NULL)

ggsave("plots/manuscript_figures/figure8_appendix.png", figure_8_app, width = 8, height = 10, dpi = 400)
ggsave("plots/manuscript_figures/figure8.png", figure_8, width = 8, height = 10, dpi = 400)


