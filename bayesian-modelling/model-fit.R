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
         episode > 88                 ~ 12)) # Levels from spreadsheet here https://www.critrolestats.com/pcstats-wm

# Centre and standardise damage to enable interpretation of "X SD increase in B1 leads to a X% inc/dec in Y"
# Healing is just log transformed

the_data$damage_c <- (log(the_data$damage) - mean(log(the_data$damage)))/sd(log(the_data$damage))
the_data$healing <- log(the_data$healing)

#---------------------PREP DATA-------------------------------------

# Extra sample size and value vectors for Stan specification

N <- nrow(the_data)
damage <- the_data$damage
healing <- the_data$healing
level_up <- the_data$level_up

# Final list ready for import into Stan models

stan_data <- list(N = N,
                  damage = damage,
                  healing = healing,
                  level_up = level_up)

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

# Image for plot

# Read in picture of The Mighty Nein for background

img <- readPNG("images/mn_light.png")

# Make plot 

p <- ggplot(the_data, aes(damage, healing)) + 
  background_image(img) +
  geom_abline(aes(intercept = `beta[1]`, slope = `beta[2]`), data = shorter_full_output, 
              alpha = 0.3, color = "#F7C9B6") + 
  geom_abline(slope = mean(shorter_full_output$`beta[2]`), 
              intercept = mean(shorter_full_output$`beta[1]`), 
              color = "#FD62AD", size = 1) + 
  geom_point(colour = "#05445E", size = 3) + 
  labs(title = "Bayesian posterior prediction of damage on healing for The Mighty Nein",
       subtitle = paste0("Plots a random ",nrow(shorter_full_output)," posterior draws. Pink line indicates mean"),
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

posterior <- as.array(mod)

# Posterior predictive distribution checks

set.seed(123)
y <- healing
yrep1 <- extract(mod)[["healing_rep"]]
samp100 <- sample(nrow(yrep1), 100)

p1 <- ppc_dens_overlay(y, yrep1[samp100, ]) 
print(p1) # Looks like the model resembles the data pretty well

p1 <- p1 +
  labs(title = "Posterior predictive check",
       subtitle = paste0("Plots a random ",length(samp100)," posterior draws")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p1)

# Test statistics

p2 <- ppc_stat(healing, yrep1, stat = 'median')
print(p2) # Might be a bit low?

p2 <- p2 +
  labs(title = "Distribution of test statistic",
       subtitle = "Test statistic = Median") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p2)

# Trace plot

color_scheme_set("mix-blue-pink")
p3 <- mcmc_trace(posterior, pars = c("beta[2]", "sigma"),
                 facet_args = list(nrow = 2, labeller = label_parsed))
print(p3)

p3 <- p3 +
  labs(title = "Trace plots for model convergence ",
       subtitle = "Test statistic = Median") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p3)

# Out-of-sample predidictive accuracy

loglik1 <- extract(mod)[["log_lik"]]
loo1 <- loo(loglik1, save_psis = TRUE)
plot(loo1)

# Probability integral transform to see whether each point sits in its predictive distribution
# Output should look uniform

ppc_loo_pit_overlay(yrep = yrep1, y = y, lw = weights(loo1$psis_object)) # Model could be calibrated better

#---------------------EXPORTS---------------------------------------

CairoPNG("output/damage-and-healing-bayes.png", 1000, 800)
ggarrange(p, p3, p1, p2, nrow = 2, ncol = 2)
dev.off()
