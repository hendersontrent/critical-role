#----------------------------------------
# This script sets out to produce a
# state-space model for healing and
# damage
#
# NOTE: This script requires setup.R and
# processing/prep-for-dmg-healing.R to
# have been run first
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 12 August 2020
#----------------------------------------

load("dam_heals.Rda")

#--------------------------- PRE PROCESSING ------------------------

# Aggregate to episode sums

raw_data <- dam_heals %>%
  gather(key = variable, value = value, c(damage, healing)) %>%
  mutate(variable = str_to_sentence(variable)) %>%
  group_by(episode, variable) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()

# Parse into healing and damage

raw_damage <- raw_data %>%
  filter(variable == "Damage")

raw_healing <- raw_data %>%
  filter(variable == "Healing")

#--------------------------- MODEL SPEC ----------------------------

# Define a function that can specify and run the Stan program

ss_model <- function(data){
  
  y <- data$value
  variable_type <- as.vector(unique(data$variable))
  
  # Data inputs for Stan program
  
  ypos <- y[!is.na(y)]
  n_pos <- sum(!is.na(y))
  indx_pos <- which(!is.na(y))
  
  # Run Stan program
  
  system.time({
    mod <- stan(data = list(y = ypos, TT = length(y), n_pos = n_pos, indx_pos = indx_pos), 
                file = "state-space/ss-model.stan",
                pars = c("sd_q", "x", "sd_r", "u", "x0"),
                iter = 4000,
                chains = 3,
                seed = 123)
  })
  
  # Extract model parameters as a dataframe
  
  pars <- rstan::extract(mod)
  the_mean <- apply(pars$x, 2, median)
  the_lower <- apply(pars$x, 2, quantile, 0.025)
  the_upper <- apply(pars$x, 2, quantile, 0.975)
  
  the_params <- data.frame(the_median = c(the_median),
                           the_lower = c(the_lower),
                           the_upper = c(the_upper)) %>%
    mutate(variable = variable_type,
           episode = row_number())
  
  return(the_params)
  
}

mod_damage <- ss_model(raw_damage)
mod_healing <- ss_model(raw_healing)

# Merge dataframes

ss_outputs <- bind_rows(mod_damage, mod_healing)

#--------------------------- DATA VIS ------------------------------

p <- raw_data %>%
  ggplot(aes(x = episode)) +
  geom_ribbon(data = ss_outputs, 
              aes(x = episode, ymin = the_lower, ymax = the_upper), fill = "#FFA384", alpha = 0.5) +
  geom_line(data = ss_outputs, 
            aes(x = episode, y = the_median), size = 1, colour = "#FFA384") +
  geom_point(data = raw_data, aes(x = episode, y = value), size = 1, colour = "black") +
  labs(title = "State space model of damage and healing by episode for The Mighty Nein",
       subtitle = "Each point is an episode sum across all characters",
       x = "Episode",
       y = "Value",
       caption = "Source: @CritRoleStats. Analysis: Orbisant Analytics.") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#E7F2F8"),
        strip.text = element_text(face = "bold", colour = "black")) +
  facet_grid(variable ~ .)
print(p)

#--------------------------- OUTPUT --------------------------------

CairoPNG("output/ss-model.png", 550, 350)
print(p)
dev.off()
