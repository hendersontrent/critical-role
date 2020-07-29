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
             iter = 1000, # 1000 seems to enable convergence, 500 does not
             seed = 123)
})

#---------------------MODEL OUTPUTS---------------------------------

summary(mod)[["summary"]][c(paste0("beta[",1:2, "]"), "sigma"),] # 1SD increase in damage leads to 0.1% increase in healing

#---------------------DATA VISUALISATION----------------------------

vis_data <- summary(mod)$summary %>%
  as_tibble()

# Posterior predictive distribution checks

set.seed(123)
y <- healing
yrep1 <- extract(mod)[["healing_rep"]]
samp100 <- sample(nrow(yrep1), 100)
ppc_dens_overlay(y, yrep1[samp100, ]) # Looks like the model resembles the data pretty well

# Test statistics

ppc_stat(healing, yrep1, stat = 'median') # Might be a bit low?

# Out-of-sample predidictive accuracy

loglik1 <- extract(mod)[["log_lik"]]
loo1 <- loo(loglik1, save_psis = TRUE)
loo1
plot(loo1)

# Probability integral transform to see whether each point sits in its predictive distribution
# Output should look uniform

ppc_loo_pit_overlay(yrep = yrep1, y = y, lw = weights(loo1$psis_object)) # Model could be calibrated better
