rm(list=ls())
library(tidyverse)
library(ggh4x)
library(ggdist)
source("code/analysis-and-figures/01.load-data.R")
source("code/analysis-and-figures/02.analysis-functions.R")

# Exclude the case of donors = 100
effects <- effects |> 
  filter(n_don != "don100")

# Create a function that will create the required figure for 3 and 4, depending on how preintervention
# outcomes are included

my_plot <- function(chosen_output_means){
  
  # bias and rmse
summaries <-
effects |> 
  filter(n_don == "don10",
         n_preint == "preint10",
         size_ob == 45,
         output_means == chosen_output_means) |> 
  compute_differences(reference = "counterfactual", pivot = TRUE, chosen_period = "post", solution = "Posible") |> 
  mutate(squared_error = error^2) |> 
  filter(!str_detect(model, "Median")) |> 
  group_by(model) |> 
  summarize(bias= mean(error),
            mse = mean(squared_error),
            rmse = sqrt(mse)) 
 

# whole distribution of effects
data <- effects |> 
  filter(n_don == "don10",
         n_preint == "preint10",
         size_ob == 45,
         output_means == chosen_output_means) |> 
  compute_differences(reference = "counterfactual", pivot = TRUE, chosen_period = "post", solution = "Posible") |> 
  filter(!str_detect(model, "Median")) 

# plot
data |> 
  ggplot(aes(x = model, y = error, fill = model)) +
  geom_hline(yintercept = 0, linetype = 848, linewidth = 1, colour = "grey50") +
  stat_slab(density = "histogram", alpha = 0.7) +
  geom_text(data = summaries, aes(x = model, y = bias, label = paste0(round(bias,0), "\n", "(", round(rmse,0), ")")),
            nudge_x = 0.3,nudge_y = 0, lineheight = .2,size = 22, family = "serif") +
  theme_classic() +
  stat_summary(fun = mean, geom = "point", size = 2, fill = "black", colour = "black") +
  theme(legend.position = "none",
        axis.title = element_text(size = 76, family = "serif"),
        axis.text.y = element_text(size = 70),
        axis.text.x = element_text(size = 70, family = "serif")) +
  labs(y = expression(Y['1t']^I - hat(Y)['1t']^N), x = NULL) +
  scale_fill_manual(values = c("navyblue", "darkgreen", "#CB2314")) +
  scale_x_discrete(guide = guide_axis(n.dodge=2),
                   labels = c("Horseshoe Priors BSCM", "Horseshoe Simplex Priors BSCM", "Standard SCM")) +
  scale_y_continuous(limits = c(-200, 200)) }

figure4 <- my_plot("out5means")
figure5 <- my_plot("outnone") +
  scale_y_continuous(limits = c(-350, 500))

ggsave("plots/manuscript_figures/figure4.png", figure4, width = 7, height = 6, dpi = 400)
ggsave("plots/manuscript_figures/figure5.png", figure5, width = 7, height = 6, dpi = 400)

