#---------------------------------------
# This script sets out to produce some
# interesting data visualisations for
# Critical Role: Wildemouth rolls
#
# NOTE: Data from - 
# https://docs.google.com/spreadsheets/d/1FFuw5c6Hk1NUlHv2Wvr5b9AElLA51KtRl9ZruPU8r9k/edit#gid=134431706
#---------------------------------------

# Load data

load("data/clean.Rda")

if (!exists(keepers)) {
  keepers <- c("keepers", "clean")
} else {
  keepers <- union(keepers, "clean")
}

#---------------------DATA VISUALISATION----------------------------

# 2D density plot

p <- clean %>%
  filter(character %in% the_nein) %>%
  ggplot(aes(x = episode, y = total_value)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  labs(title = "2-D Density of The Mighty Nein's total roll values in Wildemount by episode",
       subtitle = "Value above 100 indicates a 'Natural' value. Density legend x1000 for legibility",
       x = "Episode",
       y = "Total Roll Value",
       fill = "Density",
       caption = "Source: @CritRoleStats") +
  theme_bw() +
  scale_y_continuous(limits = c(0,120),
                     breaks = c(0,20,40,60,80,100,120)) +
  scale_x_continuous(limits = c(0,110)) +
  scale_fill_gradient(low = "#05445E", high = "#FD62AD", 
                      label = function(x) round(x*1000, digits = 1)) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(p)

# 1-D Density plot

p1 <- clean %>%
  filter(character %in% the_nein) %>%
  ggplot(aes(x = total_value)) +
  geom_density(fill = "#A0E7E5", colour = "#A0E7E5") +
  labs(title = "Distribution of The Mighty Nein's total roll values in Wildemount",
       subtitle = "Value above 100 indicates a 'Natural' value",
       x = "Total Roll Value",
       y = "Density") +
  theme_bw() +
  scale_x_continuous(limits = c(0,120),
                     breaks = c(0,20,40,60,80,100,120)) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(p1)

# Natural 20 by character

nat_20s <- clean %>%
  filter(total_value == 120) %>%
  filter(character %in% the_nein)

p2 <- nat_20s %>%
  ggplot(aes(episode, after_stat(count), fill = character)) +
  geom_density(position = "fill") +
  labs(title = "Density Distribution of The Mighty Nein's Nat20 rolls",
       subtitle = "Stat = Computed probability density estimate of Nat20 roll per episode",
       x = "Episode",
       y = "Roll Count Density",
       fill = NULL) +
  theme_bw() +
  scale_fill_manual(values = the_palette) +
  theme(panel.grid.minor = element_blank())
print(p2)

# Natural 20 dotplot by character

nat_20s_count <- nat_20s %>%
  group_by(episode, character) %>%
  summarise(counts = n()) %>%
  ungroup()

p3 <- nat_20s_count %>%
  ggplot(aes(x = episode, y = counts)) +
  geom_point(aes(colour = character), size = 3) +
  labs(title = "The Mighty Nein's Nat20 rolls in Wildemount by character and episode",
       x = "Episode",
       y = "Nat20 Count",
       colour = NULL) +
  theme_bw() +
  scale_colour_manual(values = the_palette) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(p3)

# Heatmap by character and total roll value

heat_data <- clean %>%
  filter(character %in% the_nein) %>%
  mutate(total_value = case_when(
         total_value > 100 & total_value != 101 & total_value != 120 ~ "Other Nats",  
         total_value == 101                                          ~ "Nat1",
         total_value == 120                                          ~ "Nat20",
         total_value < 5                                             ~ "<5",
         total_value >= 5 & total_value < 10                         ~ "5-10",
         total_value >= 10 & total_value < 15                        ~ "10-15",
         total_value >= 15 & total_value < 20                        ~ "15-20",
         total_value >= 20 & total_value < 25                        ~ "20-25",
         total_value >= 25 & total_value < 30                        ~ "25-30",
         total_value >= 30 & total_value < 35                        ~ "30-35",
         total_value >= 35 & total_value < 40                        ~ "35-40",
         total_value >= 40 & total_value < 45                        ~ "40-45",
         total_value >= 45 & total_value < 50                        ~ "45-50")) %>%
  mutate(total_value = factor(total_value, levels = c("Nat1", "Nat20", "Other Nats",
                                                      "<5", "5-10", "10-15", "15-20",
                                                      "20-25", "25-30", "30-35", "35-40",
                                                      "40-45", "45-50"))) %>%
  group_by(character, total_value) %>%
  summarise(counts = n()) %>%
  group_by(character) %>%
  mutate(props = (counts / sum(counts))*100) %>%
  mutate(props = round(props, digits = 1)) %>%
  ungroup()

p4 <- heat_data %>%
  ggplot(aes(x = total_value, y = character, fill = props)) +
  geom_tile(aes(width = 0.9, height = 0.9), stat = "identity") +
  geom_text(aes(x = total_value, y = character,
                label = paste0(props,"%")), colour = "white") +
  labs(title = "The Mighty Nein Wildemount total roll value breakdown by character",
       x = "Total Roll Value",
       y = NULL,
       fill = "% Total Rolls",
       caption = "Source: @CritRoleStats. Analysis: Orbisant Analytics") +
  scale_fill_gradient(low = "#A0E7E5", high = "#FD62AD",
                      label = function(x) paste0(x,"%")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())
print(p4)

#---------------------EXPORTS---------------------------------------

CairoPNG("output/MN_summary.png", 1000, 800)
ggarrange(p4,
          ggarrange(p1, p2, ncol = 2, nrow = 1),
          nrow = 2)
dev.off()
