#----------------------------------------
# This script sets out to load data
# and fit a specified Bayesian
# regression model in Stan
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 29 July 2020
#----------------------------------------

# Load packages

library(rstan)
library(bayesplot)

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
  ungroup()

# Centre and standardise damage to enable interpretation of "X SD increase in B1 leads to a X% inc/dec in Y"
# Healing is just log transformed

the_data$damage_c <- (log(the_data$damage) - mean(log(the_data$damage)))/sd(log(the_data$damage))
the_data$healing <- log(the_data$healing)

#---------------------PREP DATA-------------------------------------

# Extra sample size and value vectors for Stan specification

N <- nrow(the_data)
damage <- the_data$damage
healing <- the_data$healing
charac <- the_data$character

# Final list ready for import into Stan model

stan_data <- list(N = N,
                  damage = damage,
                  healing = healing)

#---------------------RUN MODEL-------------------------------------

system.time({
mod <- stan(data = stan_data, 
             file = "bayesian-modelling/the-model.stan",
             iter = 1000,
             seed = 123)
})

#---------------------MODEL OUTPUTS---------------------------------

summary(mod)[["summary"]][c(paste0("beta[",1:2, "]"), "sigma"),]
