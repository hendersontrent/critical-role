#---------------------------------------
# This script sets out to load and
# process the Critical Role data for 
# damage dealt and healing given
#
# This script requires setup.R to have
# been run first
#---------------------------------------

#----------------------------------------
# Author: Trent Henderson, 28 July 2020
#----------------------------------------

# Load in data

damage <- read_excel("data/Damage Dealt - Wildemount.xlsx", sheet = 1)
healing <- read_excel("data/Healing Given - Wildemount.xlsx", sheet = 1)

#-----------------
# Process and join
#-----------------

cleaner <- function(data){
  data <- data[-c(1,2), ]
  
  data <- data %>%
    clean_names() %>%
    rename(episode = 1) %>%
    gather(key = character, value = value, 2:9) %>%
    mutate(episode = case_when(
           episode == "43132.0" ~ "02-01",
           episode == "43133.0" ~ "02-02",
           episode == "43134.0" ~ "02-03",
           episode == "43135.0" ~ "02-04",
           episode == "43136.0" ~ "02-05",
           episode == "43137.0" ~ "02-06",
           episode == "43138.0" ~ "02-07",
           episode == "43139.0" ~ "02-08",
           episode == "43140.0" ~ "02-09",
           episode == "43141.0" ~ "02-10",
           episode == "43142.0" ~ "02-11",
           episode == "43143.0" ~ "02-12",
           episode == "43144.0" ~ "02-13",
           episode == "43145.0" ~ "02-14",
           episode == "43146.0" ~ "02-15",
           episode == "43147.0" ~ "02-16",
           episode == "43148.0" ~ "02-17",
           episode == "43149.0" ~ "02-18",
           episode == "43150.0" ~ "02-19",
           episode == "43151.0" ~ "02-20",
           episode == "43152.0" ~ "02-21",
           episode == "43153.0" ~ "02-22",
           episode == "43154.0" ~ "02-23",
           episode == "43155.0" ~ "02-24",
           episode == "43156.0" ~ "02-25",
           episode == "43157.0" ~ "02-26",
           episode == "43158.0" ~ "02-27",
           episode == "43159.0" ~ "02-28",
           TRUE                 ~ episode)) %>%
    mutate(episode = gsub(".*-", "", episode)) %>%
    mutate(episode = as.numeric(episode)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(character = str_to_title(character))
    
  data[data == "N/A"] <- NA
  
  return(data)
}

damage_clean <- cleaner(damage) %>%
  rename(damage = value)

healing_clean <- cleaner(healing) %>%
  rename(healing = value)

dam_heals <- damage_clean %>%
  left_join(healing_clean, by = c("episode" = "episode", "character" = "character"))

#---------------------STORAGE---------------------------------------

save(dam_heals, file = "data/dam_heals.Rda")
