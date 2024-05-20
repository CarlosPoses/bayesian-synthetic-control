showtext::showtext.auto()
library(tidyverse)
#source("presentation/plots_presentation.R")
annotation_size <- 22
positive_effect_series <- data.frame(
  year = 1989:2000,
  cigsale = c(87, 86, 89, 91, 94, 98, 104, 108, 107, 108, 112, 114)
)
data <- readRDS("data/proposition99.rds")

weights <- tibble(state = c("Colorado", "Connecticut","Montana", "Nevada", "Utah"),
                  weights = c(0.164,0.069, 0.199, 0.234, 0.334))

set.seed(321)

synt_cont <-
data %>% 
  filter(state %in% c("Colorado", "Connecticut","Montana", "Nevada", "Utah"))  |> 
  left_join(weights, by = "state") %>% 
  mutate(contribution = weights*cigsale) %>% 
  group_by(year) %>% 
  mutate(synt_control = sum(contribution)) |> 
  filter(state == "Colorado") %>% 
  mutate(state = "synthetic_control") %>% 
  select(-cigsale) %>% 
  rename(cigsale = synt_control) %>% 
  mutate(synt_cont_lower = cigsale - 10 + rnorm(1, mean = 0, sd = 3),
         synt_cont_upper = cigsale + 9 + rnorm(1, mean =, sd = 3)) 


#### PanelA
panel_a <- ggplot(data) +
  # themes
  theme_classic() +
  theme(text = element_text(family = "Serif"),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 62),
        axis.text = element_text(size = 54),
        plot.caption = element_text(size = 48, face = "italic"),
        plot.margin = margin(2,2,0,0, unit = "cm")) +
  # Scale and coordinates
  scale_x_continuous(breaks = seq(1970, 2000, by = 5)) +
  coord_cartesian(ylim = c(0,200),
                  xlim = c(1970, 2000),
                  clip = "off") +
  # labs
  labs(y = "Cigarette sales*",
       caption = "*Packs per capita",
       x = NULL) +
  # vertical line
  geom_vline(aes(xintercept = 1989),
             linetype = "dashed") +
  # counterfactual
  geom_line(data = positive_effect_series, 
            aes(x = year, y = cigsale),
            color = "#CB2314",
            linewidth = 1.5,
            linetype = "dashed",
            alpha = 0.3) +
  # factual
  geom_line(data = data %>% filter(state == "California"), 
            aes(x = year, y = cigsale),
            color = "#CB2314",
            linewidth = 1.5) +
  # anotations
  ## Anti-tobacco law
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
  ## No-Intervention California
  annotate(geom = "text", x = 1996, y = 135, 
           label = expression(atop(plain("No-intervention"), plain("California ") * plain("(") * Y['1t']^N * plain(")"))),
           colour = "#CB2314",
           size = annotation_size,
           family = "serif",
           alpha = .3,
           parse = TRUE) +
  # Causal effect
  annotate("text", x = 2000.5, y = 80, label = "True \n causal \n effect", 
           colour = "#0B775E",
           size = annotation_size,
           family = "serif",
           lineheight = .3) +
  geom_curve(arrow = arrow(length = unit(0.03, "npc"), ends = "both"),
             curvature = 0,
             size = 1,
             colour = "#0B775E",
             aes(x = 1998, y = 56, xend = 1998, yend = 104)) 
 

panel_b <- #### PanelA
 ggplot(data) +
  # themes
  theme_classic() +
  theme(text = element_text(family = "Serif"),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 62),
        axis.text = element_text(size = 54),
        plot.caption = element_text(size = 48, face = "italic"),
        plot.margin = margin(2,2,0,0, unit = "cm")) +
  # Scale and coordinates
  scale_x_continuous(breaks = seq(1970, 2000, by = 5)) +
  coord_cartesian(ylim = c(0,200),
                  xlim = c(1970, 2000),
                  clip = "off") +
  # labs
  labs(y = "Cigarette sales*",
       caption = "*Packs per capita",
       x = NULL) +
  # vertical line
  geom_vline(aes(xintercept = 1989),
             linetype = "dashed") +
  # anotations
  
  # factual (California)
  geom_line(data = data %>% filter(state == "California"), 
          aes(x = year, y = cigsale),
          color = "#CB2314",
          linewidth = 1.5,
          alpha = 1) +
  ## Other states
  geom_line(data = data %>% filter(state %in% c("Colorado", "Connecticut","Montana", "Nevada", "Utah")),
            aes(x = year, group = state, y = cigsale, color = state),
            alpha = 0.25,
            linetype = "91",
            color = "#0D1495",
            linewidth = 0.6) +
  annotate(geom = "text", x = 1972, y = 90, label = "Y['jt']", colour = "#0D1495",
           size = annotation_size,
           family = "serif",
           alpha = 0.3,
           parse = TRUE) +
  theme(legend.position = "none") +
  # Synthetic control  
  ## Line
  geom_line(data = synt_cont,
            aes(x = year, y = cigsale),
          #  size = 0.5,
          linewidth = 2,
          alpha = 1,
            linetype = "52",
            color = "#0D1495") +
  # Annotations
  annotate(geom = "text", x = 1976, y = 150, 
           label = expression(plain("Synthetic ") * plain("control  ") *
                                   plain("(") * hat(Y['1t']^N)* plain(")")),
           colour = "#0D1495",
           size = annotation_size,
           family = "serif",
           lineheight = 0.3,
           alpha = 1,
           parse = TRUE)+
  # Causal effect
  annotate("text", x = 2002.8, y = 78, label = "Estimated \n causal \n effect", 
           colour = "#0B775E",
           size = annotation_size,
           family = "serif",
           lineheight = .3) +
  geom_curve(arrow = arrow(length = unit(0.02, "npc")),
             curvature = -0.35,
             colour = "#0B775E",
             size = 0.3,
             aes(x = 2002.35, y = 61, xend = 1998.75, yend = 63.5),
             angle = 80) +
  geom_curve(arrow = arrow(length = unit(0.02, "npc"), ends = "both"),
             curvature = 0,
             size = 1,
             colour = "#0B775E",
             aes(x = 1998, y = 56, xend = 1998, yend = 71)) 

library(patchwork)

figure1 <-  panel_a+panel_b + 
plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 90))

ggsave(filename = "plots/manuscript_figures/figure1.png", plot = figure1, width = 18, height = 7)
ggsave(filename = "plots/manuscript_figures/figure1_panela.png", plot = panel_a, width = 9, height = 7)
ggsave(filename = "plots/manuscript_figures/figure1_panelb.png", plot = panel_b, width = 9, height = 7)