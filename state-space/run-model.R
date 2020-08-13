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

load("data/dam_heals.Rda")

#--------------------------- PRE PROCESSING ------------------------

# Aggregate to episode sums

raw_data <- dam_heals %>%
  gather(key = variable, value = value, c(damage, healing)) %>%
  mutate(variable = str_to_sentence(variable)) %>%
  group_by(episode, variable) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(variable) %>%
  mutate(n = n(), sd = sd(value)) %>%
  ungroup()

#--------------------------- MODEL SPEC ----------------------------

the_vars <- unique(raw_data$variable)

some_list <- list()

for(i in the_vars){
  
  shorter <- raw_data %>%
    filter(variable == i)

  d1 <- list(
    mu_start = first(shorter$value),
    n_eps = nrow(shorter),
    y_values = shorter$value,
    sigma = unique(shorter$sd)
  )
  
  system.time({
    mod <- stan(file = "state-space/ss-model.stan", data = d1, iter = 4000, control = list(max_treedepth = 20))
  })
  
  ex <- as.data.frame(rstan::extract(mod, "mu"))
  
  outs <- ex %>%
    gather(key = episode, value = value, 1:105) %>%
    mutate(episode = gsub("mu.", "\\1", episode)) %>%
    mutate(episode = as.numeric(episode)) %>%
    group_by(episode) %>%
    summarise(mean = mean(value),
              upper = quantile(value, 0.975),
              lower = quantile(value, 0.025)) %>%
    ungroup() %>%
    mutate(variable = i)
  
  some_list[[i]] <- outs

}

full_models <- rbindlist(some_list, use.names = TRUE)

#--------------------------- DATA VIS ------------------------------

# Read in picture of The Mighty Nein for background

img <- readPNG("images/mn_light.png")

the_palette <- c("#F84791", "#FFA384")

p <- raw_data %>%
  ggplot(aes(x = episode)) +
  background_image(img) +
  geom_ribbon(data = full_models, 
              aes(x = episode, ymin = lower, ymax = upper, fill = variable), alpha = 0.4) +
  geom_line(data = full_models, 
            aes(x = episode, y = mean, colour = variable), size = 1.1) +
  geom_point(data = raw_data, aes(x = episode, y = value), size = 1.5, colour = "black") +
  labs(title = "Bayesian state space model of The Mighty Nein's damage and healing by episode",
       subtitle = "Each point is an episode sum across all characters. Coloured shading indicates 95% credible interval.",
       x = "Episode",
       y = "Episode Sum Value",
       caption = "Source: @CritRoleStats. Analysis: Orbisant Analytics.") +
  theme_bw() +
  scale_colour_manual(values = the_palette) +
  scale_fill_manual(values = the_palette) +
  guides(fill = FALSE) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#E7F2F8"),
        strip.text = element_text(face = "bold", colour = "black")) +
  facet_grid(variable ~.)
print(p)

#--------------------------- OUTPUT --------------------------------

CairoPNG("output/ss-model.png", 700, 500)
print(p)
dev.off()
