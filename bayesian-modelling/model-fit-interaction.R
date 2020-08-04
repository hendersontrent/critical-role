#----------------------------------------
# This script sets out to load data
# and fit specified Bayesian
# regression models in Stan
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 4 August 2020
#----------------------------------------

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

#---------------------PREP DATA-------------------------------------

# Extra sample size and value vectors for Stan specification

N <- nrow(the_data)
damage <- the_data$damage
healing <- the_data$healing
proficiency <- as.numeric(the_data$proficiency_group_binary)

# Final list ready for import into Stan models

stan_data <- list(N = N,
                  damage = damage,
                  healing = healing,
                  proficiency = proficiency)

#---------------------RUN MODELS------------------------------------

system.time({
  mod <- stan(data = stan_data, 
              file = "bayesian-modelling/the-model-interaction.stan",
              iter = 1000,
              chains = 4,
              seed = 123)
})

#---------------------MODEL OUTPUTS---------------------------------

summary(mod)[["summary"]][c(paste0("beta[",1:4, "]"), "sigma"),]

#---------------------MODEL OUTPUTS---------------------------------


