#----------------------------------------
# This script sets out to load data
# and fit specified Bayesian
# regression models in Stan and compare
# whether a model with an interaction
# term performs better than one without
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 29 July 2020
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
  ungroup()

# Number of episodes

eps <- max(the_data$episode)

the_data <- the_data %>%
  mutate(indicator = case_when(
    episode < eps ~ 0,
    TRUE          ~ 1))

# Centre and standardise damage to enable interpretation of "X SD increase in B1 leads to a X% inc/dec in Y"
# Healing is just log transformed

the_data$damage_c <- (log(the_data$damage) - mean(log(the_data$damage)))/sd(log(the_data$damage))
the_data$healing <- log(the_data$healing)

#---------------------PREP DATA-------------------------------------

# Extra sample size and value vectors for Stan specification

N <- nrow(the_data)
damage <- the_data$damage
healing <- the_data$healing
episode <- the_data$indicator

# Final list ready for import into Stan models

stan_data <- list(N = N,
                  damage = damage,
                  healing = healing,
                  episode = episode)

#---------------------RUN MODELS------------------------------------

system.time({
mod <- stan(data = stan_data, 
             file = "bayesian-modelling/the-model.stan",
             iter = 1000, # 1000 seems to enable convergence, 500 does not
             chains = 4,
             seed = 123)
})

#---------------------MODEL OUTPUTS---------------------------------

summary(mod)[["summary"]][c(paste0("beta[",1:2, "]"), "sigma"),] # 1SD increase in damage leads to 0.1% increase in healing

#---------------------DATA VISUALISATION----------------------------

#-----------------
# Regression plots
#-----------------

# Extract model outputs

full_output <- as.data.frame(mod) %>%
  mutate(ID = row_number())

# Take a random sample to make data vis cleaner

sample_size <- floor(0.25 * nrow(full_output))
set.seed(123)
an_indicator <- sample(seq_len(nrow(full_output)), size = sample_size)
shorter_full_output <- full_output[an_indicator,]

# Make plot 

p <- ggplot(the_data, aes(damage, healing)) + 
  geom_abline(aes(intercept = `beta[1]`, slope = `beta[2]`), data = shorter_full_output, 
              alpha = 0.1, color = "#A0E7E5") + 
  geom_abline(slope = mean(shorter_full_output$`beta[2]`), 
              intercept = mean(shorter_full_output$`beta[1]`), 
              color = "#FD62AD", size = 1) + 
  geom_point(colour = "#05445E", size = 2) + 
  labs(title = "Bayesian posterior prediction of damage on healing for The Mighty Nein",
       subtitle = paste0("Plots a random ",nrow(shorter_full_output)," posterior draws. Pink line indicates mean."),
       x = "Damage",
       y = "log(Healing)",
       caption = "Source: @CritRoleStats. Analysis: Orbisant Analytics") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p)

#------------------
# Model comparisons
#------------------

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
plot(loo1)

# Probability integral transform to see whether each point sits in its predictive distribution
# Output should look uniform

ppc_loo_pit_overlay(yrep = yrep1, y = y, lw = weights(loo1$psis_object)) # Model could be calibrated better

#---------------------EXPORTS---------------------------------------

CairoPNG("output/damage-and-healing-bayes.png", 800, 600)
print(p)
dev.off()

