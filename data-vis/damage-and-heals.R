#---------------------------------------
# This script sets out to produce some
# interesting data visualisations for
# Critical Role: Wildemouth rolls
#---------------------------------------

#----------------------------------------
# Author: Trent Henderson, 28 July 2020
#----------------------------------------

# Load data

load("data/dam_heals.Rda")

#---------------------DATA VISUALISATION----------------------------

healing_max <- max(dam_heals$healing) # For locking cartesian coordinates in the plot

# Render plot

dam_heal_plot <- dam_heals %>%
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
  ggplot(aes(x = damage, y = healing, colour = character)) +
  geom_smooth(formula = y ~ s(x), method = "gam", aes(group = 1),
              fill = "#FEB06A", colour = "#FEB06A") +
  geom_point(size = 2) +
  labs(title = "The Mighty Nein damage dealt and healing given",
       subtitle = "Each point is a non-zero episode sum for damage and healing per character",
       x = "Damage Dealt",
       y = "Healing Given",
       colour = NULL,
       caption = "Episodes where both damage and healing were 0 for a character were removed. Outliers also removed.") +
  coord_cartesian(ylim = c(0, healing_max))  + # Constrains geom_smooth to 0-max range
  theme_bw() +
  scale_colour_manual(values = the_palette) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(dam_heal_plot)
