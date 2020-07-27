#----------------------------------------
# This script sets out to load all 
# things required for the project
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 23 July 2020
#----------------------------------------

# Load packages

library(tidyverse)
library(readxl)
library(data.table)
library(janitor)
library(scales)
library(Cairo)
library(ggpubr)
library(nnet)
library(effects)
library(forecast)

# Turn off scientific notation

options(scipen = 999)

# Load functions that are useful

keepers <- c("keepers")

source("R/cleanup_env.R")

if (!exists(keepers)) {
  keepers <- c("keepers", "cleanup_env")
} else {
  keepers <- union(keepers, "cleanup_env")
}

# List of characters

the_nein <- c("Beau", "Caduceus", "Caleb", "Fjord", "Jester", "Veth", "Yasha", 
              "Molly")

if (!exists(keepers)) {
  keepers <- c("keepers", "the_nein")
} else {
  keepers <- union(keepers, "the_nein")
}

# Define a vector of nice colours for each character to use in plotting

the_palette <- c("Beau" = "#A0E7E5",
                 "Caduceus" = "#75E6DA",
                 "Caleb" = "#189AB4",
                 "Fjord" = "#05445E",
                 "Jester" = "#9571AB",
                 "Veth" = "#FD62AD",
                 "Yasha" = "#F7C9B6",
                 "Molly" = "#E7625F")

if (!exists(keepers)) {
  keepers <- c("keepers", "the_palette")
} else {
  keepers <- union(keepers, "the_palette")
}

# Create an output folder if none exists:

if(!dir.exists('output')) dir.create('output')