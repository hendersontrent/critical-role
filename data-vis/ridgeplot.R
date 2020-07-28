#---------------------------------------
# This script sets out to produce some
# interesting data visualisations for
# Critical Role: Wildemouth rolls
#---------------------------------------

#----------------------------------------
# Author: Trent Henderson, 28 July 2020
#----------------------------------------

# Load data

load("data/clean.Rda")

if (!exists(keepers)) {
  keepers <- c("keepers", "clean")
} else {
  keepers <- union(keepers, "clean")
}

#---------------------DATA VISUALISATION----------------------------

the_dens <- clean %>%
  filter(character %in% the_nein) %>%
  mutate(character = case_when(
    character == "Nott" ~ "Veth/Nott",
    character == "Veth" ~ "Veth/Nott",
    TRUE                ~ character)) %>%
  filter(total_value < 100) %>%
  ggplot(aes(x = total_value, y = character, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  labs(title = "Distribution of The Mighty Nein's roll values",
       subtitle = "Excludes Nat1s and Nat20s",
       x = "Total Roll Value",
       y = NULL,
       fill = "Roll value") +
  theme_bw() +
  scale_x_continuous(limits = c(0,50),
                     breaks = c(0,10,20,30,40,50)) +
  scale_fill_gradient(low = "#A0E7E5", high = "#FD62AD") +
  theme(panel.grid.minor = element_blank())
print(the_dens)
