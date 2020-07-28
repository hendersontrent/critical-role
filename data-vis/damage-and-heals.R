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

dam_heal_plot <- dam_heals %>%
  mutate(character = case_when(
    character == "Nott_veth"  ~ "Veth/Nott",
    character == "Mollymauk"  ~ "Molly",
    character == "Beauregard" ~ "Beau",
    TRUE                      ~ character)) %>%
  drop_na() %>%
  ggplot(aes(x = damage, y = healing, colour = character)) +
  geom_smooth(formula = y ~ s(x), method = "gam", aes(group = 1)) +
  geom_point(size = 2) +
  labs(title = "The Mighty Nein damage dealt and healing given",
       subtitle = "Each point is a single character incident",
       x = "Damage Dealt",
       y = "Healing Given",
       colour = NULL) +
  theme_bw() +
  scale_colour_manual(values = the_palette) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(dam_heal_plot)
