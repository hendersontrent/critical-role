#----------------------------------------
# This script sets out to load data
# and fit specified Bayesian
# regression models in Stan and compare
# whether a model with an interaction
# term performs better than one without
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 31 July 2020
#----------------------------------------

library(rstanarm)

#---------------------DATA LOADS------------------------------------

# Load data

load("data/dam_heals.Rda")

the_data <- dam_heals %>%
  mutate(character = case_when(
    character == "Nott_veth"  ~ "Veth/Nott",
    character == "Mollymauk"  ~ "Molly",
    character == "Beauregard" ~ "Beau",
    TRUE                      ~ character)) %>%
  filter(damage != 0 & healing != 0) %>%
  filter(damage < 400) %>%
  drop_na() %>%
  group_by(episode, character) %>%
  summarise(damage = sum(damage),
            healing = sum(healing)) %>%
  ungroup() %>%
  mutate(level_up = case_when(
    episode == 1                 ~ 2,
    episode > 2 & episode <= 5   ~ 3,
    episode > 5 & episode <= 13  ~ 4,
    episode > 13 & episode <= 18 ~ 5,
    episode > 18 & episode <= 30 ~ 6,
    episode > 30 & episode <= 41 ~ 7,
    episode > 41 & episode <= 49 ~ 8,
    episode > 49 & episode <= 58 ~ 9,
    episode > 58 & episode <= 76 ~ 10,
    episode > 76 & episode <= 88 ~ 11,
    episode > 88                 ~ 12)) %>% # Levels from spreadsheet here https://www.critrolestats.com/pcstats-wm
  mutate(proficiency_bonus = case_when(
    level_up < 5                   ~ "+2",
    level_up >= 5 & level_up < 9   ~ "+3",
    level_up >= 10 & level_up < 13 ~ "+4",
    level_up >= 13 & level_up < 17 ~ "+5",
    level_up >= 17                 ~ "+6")) %>%
  mutate(proficiency_group = case_when(
    proficiency_bonus == "+2" | proficiency_bonus == "+3" ~ "Bonus of <=3",
    TRUE                                                  ~ "Bonus of 4-6")) %>%
  mutate(proficiency_group_binary = case_when(
    proficiency_group == "Bonus of <=3" ~ 0,
    TRUE                                ~ 1)) %>%
  mutate(proficiency_group = as.factor(proficiency_group),
         proficiency_bonus = as.factor(proficiency_bonus),
         proficiency_group_binary = as.factor(proficiency_group_binary))

# Centre and standardise damage to enable interpretation of "X SD increase in B1 leads to a X% inc/dec in Y"
# Healing is just log transformed

the_data$damage_c <- (log(the_data$damage) - mean(log(the_data$damage)))/sd(log(the_data$damage))
the_data$healing <- log(the_data$healing)

#---------------------RUN MODELS------------------------------------

m1 <- rstanarm::stan_glm(healing ~ 1 + damage + proficiency_group + damage*proficiency_group, 
                         data = the_data,
                         family = gaussian(),
                         algorithm = "sampling",
                         chains = 4,
                         seed = 123)

#---------------------DATA VISUALISATION----------------------------

# Extras

thedraws <- 200
img <- readPNG("images/mn_lesslight.png")

# Plot

newplot <- the_data %>%
  group_by(proficiency_group) %>%
  data_grid(damage = seq_range(damage, n = 101)) %>%
  add_fitted_draws(m1, n = thedraws) %>%
  ggplot(aes(x = damage, y = healing, colour = ordered(proficiency_group))) +
  background_image(img) +
  geom_line(aes(y = .value, group = paste(proficiency_group, .draw)), alpha = .2) +
  geom_point(data = the_data, size = 3) +
  labs(title = "Bayesian posterior prediction of damage on healing for The Mighty Nein by level proficiency bonus",
       subtitle = paste0("Plots a random ",thedraws," posterior draws"),
       x = "Damage",
       y = "log(Healing)",
       colour = "Proficiency Bonus",
       caption = "Source: @CritRoleStats. Analysis: Orbisant Analytics") +
  scale_colour_manual(values = c("#05445E", "#FD62AD")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "#05445E"),
        axis.title = element_text(colour = "#05445E", face = "bold"),
        legend.text = element_text(colour = "#05445E"),
        legend.title = element_text(colour = "#05445E"),
        plot.caption = element_text(colour = "#05445E"),
        plot.title = element_text(colour = "#05445E", face = "bold"),
        plot.subtitle = element_text(colour = "#05445E"))
print(newplot)

# Other plots

other_plot <- ppc_dens_overlay(y = m1$y, 
                 yrep = posterior_predict(m1, draws = 500)) +
  labs(title = "Distribution of model-simulated data compared to the actual data",
       subtitle = "Posterior draws = 500",
       caption = "Source: @CritRoleStats. Analysis: Orbisant Analytics") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect("#05445E"),
        strip.text = element_text(colour = "white", face = "bold"),
        axis.text = element_text(colour = "#05445E"),
        axis.title = element_text(colour = "#05445E", face = "bold"),
        legend.text = element_text(colour = "#05445E"),
        legend.title = element_text(colour = "#05445E"),
        plot.caption = element_text(colour = "#05445E"),
        plot.title = element_text(colour = "#05445E", face = "bold"),
        plot.subtitle = element_text(colour = "#05445E"))
print(other_plot)

another_plot <- m1 %>% 
  posterior_predict(draws = 500) %>%
  ppc_stat_grouped(y = the_data$healing, 
                   group = the_data$proficiency_group, 
                   stat = "median") +
  labs(title = "Distribution of model-simulated test statistic compared to the real statistic",
       subtitle = "Posterior draws = 500",
       caption = "Source: @CritRoleStats. Analysis: Orbisant Analytics") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect("#05445E"),
        strip.text = element_text(colour = "white", face = "bold"),
        axis.text = element_text(colour = "#05445E"),
        axis.title = element_text(colour = "#05445E", face = "bold"),
        legend.text = element_text(colour = "#05445E"),
        legend.title = element_text(colour = "#05445E"),
        plot.caption = element_text(colour = "#05445E"),
        plot.title = element_text(colour = "#05445E", face = "bold"),
        plot.subtitle = element_text(colour = "#05445E"))
print(another_plot)

#---------------------EXPORTS---------------------------------------

# Single main plot

CairoPNG("output/int-bayes-single.png", 700, 450)
print(newplot)
dev.off()

# Matrix of plots

CairoPNG("output/damage-and-healing-bayes-int.png", 1000, 800)
ggarrange(other_plot, another_plot, nrow = 2, ncol = 1)
dev.off()
